package net.zyuiop.philofinder

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.{TwitterRestClient, TwitterStreamingClient}
import net.zyuiop.philofinder.ShortestPathFinder.{Status, functionnalBfs}
import net.zyuiop.philofinder.Twitter.ComputedPath

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Source, StdIn}
import scala.reflect.io.File
import scala.util.{Failure, Success}

/**
  * @author Louis Vialar
  */
object Twitter {
  def main(args: Array[String]): Unit = {
    val lang = args(0)
    val target = System.getenv("TARGET_PAGE")
    val default = System.getenv("DEFAULT_SOURCE")
    val username = System.getenv("TWITTER_USERNAME")

    val client = TwitterRestClient()

    new Twitter(new WikiBrowser(lang), client, TwitterStreamingClient(), target, default, username).mainLoop()
  }

  case class ComputedPath(source: Article, path: List[Article])

  case class State(readyUser: Queue[ComputedPath], readyAutomatic: Queue[ComputedPath])

}

class Twitter(browser: WikiBrowser, client: TwitterRestClient, streaming: TwitterStreamingClient, target: String, default: String, username: String) {
  val readyUser: mutable.Queue[String] = mutable.Queue()
  val readyAuto: mutable.Queue[String] = mutable.Queue()
  val waitingUser: mutable.Queue[Article] = mutable.Queue()
  val ex: ScheduledExecutorService = Executors.newScheduledThreadPool(4)
  var lastTweet: Long = 0
  var lastDmCheck: Long = 0

  def mainLoop(): Unit = {
    load()

    ex.scheduleAtFixedRate(() => computeNextPath(), 0, 10, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => tweetNext(), 60, 10, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => logState(), 0, 1, TimeUnit.MINUTES)
    ex.scheduleAtFixedRate(() => save(), 0, 1, TimeUnit.MINUTES)

    println("Started twitter bot with target " + target)

    val stream = streaming.userEvents()({
      case t: Tweet =>
        if (t.in_reply_to_screen_name.getOrElse("").equalsIgnoreCase(username)
          && t.in_reply_to_status_id.isEmpty
          && t.in_reply_to_status_id_str.isEmpty) {
          val tweetContent = t.text.replaceAll("@[a-zA-Z0-9_-]+", "").trim
          println(tweetContent)
          try {
            val article = browser.getRealArticle(tweetContent)
            client.createDirectMessage(t.user.get.screen_name, "J'ai bien reçu votre demande! Je vais chercher la page Adolf Hitler en partant de " + article.name + " [[" + article.url + "]]")
            waitingUser.enqueue(article)
          } catch {
            case _ => client.favoriteStatus(t.id)
          }
        }
    })

    var cont = true
    while (cont) {
      val q = StdIn.readLine("Quit ? (type y to quit)")
      if (q == "y")
        cont = false
    }

    ex.shutdownNow()
    stream.andThen({
      case Success(twitterStream) => twitterStream.close()
    })
  }

  def logState(): Unit = {
    println("----- STATE LOG ------")
    println("User queue: " + readyUser.length)
    println("Auto queue: " + readyAuto.length)
    println("User waiting: " + waitingUser.length)
  }

  def buildPath(article: Article, consummer: String => Unit): Unit = {
    val start = article
    val route = functionnalBfs(browser, target, Status(Queue(start), Map(start.url -> null.asInstanceOf[Article])))

    if (route.isEmpty) return

    val tweet = buildTweet(ComputedPath(start, route))

    println("Generated tweet, queueing.")

    if (isTweetable(tweet)) consummer(tweet)
    else println("!! path not tweetable " + route)
  }

  def computeNextPath(): Unit = {
    println(" -> Computing a path")
    if (waitingUser.nonEmpty) {
      val start = waitingUser.dequeue
      buildPath(start, e => readyUser.enqueue(e))
    } else if (readyAuto.lengthCompare(50) < 0) {
      val start = browser.getRealArticle(default)
      buildPath(start, e => readyAuto.enqueue(e))
    }
  }

  def isTweetable(tweet: String): Boolean = tweet.length < 240

  def buildTweet(computedPath: ComputedPath): String = {
    def printRoute(hop: Int, list: List[Article], acc: String): String = {
      val hopName = if (hop == 0) "\uD83C\uDFE0" else "↳"

      if (list.isEmpty) acc
      else printRoute(hop + 1, list.tail, acc + "\n" + hopName + " " + list.head.name)
    }

    printRoute(0, computedPath.path, computedPath.source.name + " vers " + target + " :") + "\nTotal : " + (computedPath.path.length - 1) + " pages"
  }

  def tweetNext(): Unit = {
    if (lastTweet + 300000 > System.currentTimeMillis)
      return
    println("Tweeting!")

    if (readyUser.nonEmpty) {
      lastTweet = System.currentTimeMillis()
      tweet(readyUser.dequeue())
    } else if (readyAuto.nonEmpty) {
      lastTweet = System.currentTimeMillis()
      tweet(readyAuto.dequeue())
    } else {
      println("!! Nothing to tweet !!")
    }
  }

  def tweet(tweet: String): Unit = {
    client.createTweet(status = tweet).andThen {
      case Success(t: Tweet) => println("Tweeted: " + tweet)
      case Failure(ex: Throwable) => ex.printStackTrace()
    }
  }

  val saveSeparator = "<!!TWEETS_SEPARATOR!!>"

  def save(): Unit = {
    println("Saving data...")
    val tweetsFile = File("tweets.tst")
    val requestsFile = File("requests.tst")
    val tweets = readyUser.mkString(saveSeparator)
    val requests = waitingUser.map(q => q.url).mkString(saveSeparator)
    requestsFile.writeAll(requests)
    tweetsFile.writeAll(tweets)

    println("Saved data!")
  }

  def load(): Unit = {
    val tweetsFile = File("tweets.tst")
    val requestsFile = File("requests.tst")

    if (tweetsFile.exists) {
      val data = Source.fromFile("tweets.tst")
      if (data.nonEmpty)
        data.mkString.split(saveSeparator).foreach(tweet => readyUser.enqueue(tweet))
    }

    if (requestsFile.exists) {
      val data = Source.fromFile("requests.tst")

      if (data.nonEmpty)
        data.mkString.split(saveSeparator).foreach(request => waitingUser.enqueue(browser.getRealArticle(request)))
    }

  }
}