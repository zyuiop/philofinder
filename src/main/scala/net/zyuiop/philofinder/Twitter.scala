package net.zyuiop.philofinder

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.exceptions.TwitterException
import com.danielasfregola.twitter4s.{TwitterRestClient, TwitterStreamingClient}
import com.typesafe.scalalogging.LazyLogging
import net.zyuiop.philofinder.ShortestPathFinder.{Status, functionnalBfs}
import net.zyuiop.philofinder.Twitter.ComputedPath

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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

class Twitter(browser: WikiBrowser, client: TwitterRestClient, streaming: TwitterStreamingClient, target: String, default: String, username: String) extends LazyLogging {
  val readyUser: mutable.Queue[String] = mutable.Queue()
  val readyAuto: mutable.Queue[String] = mutable.Queue()
  val waitingUser: mutable.Queue[Article] = mutable.Queue()
  val ex: ScheduledExecutorService = Executors.newScheduledThreadPool(4)
  var lastTweet: Long = 0
  var lastDmCheck: Long = 0
  var tweeting: Boolean = false

  def mainLoop(): Unit = {
    load()

    ex.scheduleAtFixedRate(() => computeNextPath(), 0, 10, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => tweetNext(), 20, 10, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => logState(), 0, 1, TimeUnit.MINUTES)
    ex.scheduleAtFixedRate(() => save(), 0, 1, TimeUnit.MINUTES)

    logger.info("Started twitter bot with target " + target)

    val stream = streaming.userEvents()({
      case t: Tweet =>
        if (t.in_reply_to_screen_name.getOrElse("").equalsIgnoreCase(username)
          && t.in_reply_to_status_id.isEmpty
          && t.in_reply_to_status_id_str.isEmpty) {
          val tweetContent = t.text.replaceAll("@[a-zA-Z0-9_-]+", "").trim
          println(tweetContent)
          try {
            val article = browser.getRealArticle(tweetContent)
            repeatIfFailing("Reply ok " + t.id,
              client.createTweet("@" + t.user.get.screen_name + " Recherche prise en compte ! J'irai chercher " + article.name,
                in_reply_to_status_id = Option.apply(t.id)))
            waitingUser.enqueue(article)
          } catch {
            case e: Throwable =>
              e.printStackTrace()
              repeatIfFailing("Like " + t.id, client.favoriteStatus(t.id))
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

  def repeatIfFailing(taskName: String, runnable: => Future[Any], retryCnt: Int = 0): Unit = {
    if (retryCnt > 10) {
      logger.error(" !! Task " + taskName + " failed after 10 retries")
      return
    }

    runnable.onComplete {
      case Failure(ex: Throwable) =>
        logger.error("  !! Task " + taskName, ex)
        repeatIfFailing(taskName, runnable, retryCnt + 1)
      case _ =>
    }
  }

  def logState(): Unit = {
    logger.info("----- STATE LOG ------")
    logger.info("User queue: " + readyUser.length)
    logger.info("Auto queue: " + readyAuto.length)
    logger.info("User waiting: " + waitingUser.length)
  }

  def buildPath(article: Article, consummer: String => Unit, onError: => Unit = () => ()): Unit = {
    val start = article
    try {
      val route = functionnalBfs(browser, target, Status(Queue(start), Map(start.url -> null.asInstanceOf[Article])))

      if (route.isEmpty) return

      val tweet = buildTweet(ComputedPath(start, route))

      logger.info(" -> Generated tweet, queueing " + route)

      if (isTweetable(tweet)) consummer(tweet)
      else logger.warn(" !! path not tweetable")
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        onError
    }
  }

  def computeNextPath(): Unit = {
    if (waitingUser.nonEmpty) {
      logger.info("-> Computing a user path")
      val start = waitingUser.dequeue
      buildPath(start, e => readyUser.enqueue(e), () => waitingUser.enqueue(start))
    } else if (readyAuto.lengthCompare(25) < 0) {
      logger.info("-> Computing a random path " + readyAuto.length)
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

    printRoute(0, computedPath.path, computedPath.source.name + " vers " + target + " :\n") + "\n\nTotal : " + (computedPath.path.length - 1) + " pages"
  }

  def tweetNext(): Unit = {
    if (lastTweet + 600000 > System.currentTimeMillis)
      return
    logger.info("-> Trying to tweet")

    if (tweeting) {
      logger.warn(" !! An other tweet is being processed. Cancelling.")
      return
    }

    tweeting = true

    if (readyUser.nonEmpty) {
      tweet(readyUser.dequeue())
    } else if (readyAuto.nonEmpty) {
      tweet(readyAuto.dequeue())
    } else {
      logger.error(" !! Nothing to tweet")
      tweeting = false
    }
  }

  def tweet(tweet: String): Unit = {
    client.createTweet(status = tweet).onComplete(r => {
      r match {
        case Success(t: Tweet) =>
          logger.info(" :) Tweeted: " + tweet.replaceAll("\n", "<nl>"))
          lastTweet = System.currentTimeMillis()
        case Failure(ex: TwitterException) if ex.errors.errors.head.code == 187 =>
          // Duplicate status
          logger.warn(" !! Error tweeting: " + tweet.replaceAll("\n", "<nl>") + " ==> Status is duplicate")
        case Failure(ex: Throwable) =>
          // Unknown error
          logger.error(" !! Error tweeting", ex)
          readyUser.enqueue(tweet) // re-enqueue tweet to avoid it being discarded
      }
      tweeting = false
      save()
    })
  }

  def save(): Unit = {
    SavingManager.save[String]("tweets.tst", readyUser)
    SavingManager.save[String]("auto-tweets.tst", readyAuto)
    SavingManager.save[Article]("requests.tst", waitingUser, _.url)
  }

  def load(): Unit = {
    SavingManager.load[String]("tweets.tst").foreach(readyUser.enqueue(_))
    SavingManager.load[String]("auto-tweets.tst").foreach(readyAuto.enqueue(_))
    SavingManager.load[Article]("requests.tst", browser.getRealArticle).foreach(waitingUser.enqueue(_))
  }
}