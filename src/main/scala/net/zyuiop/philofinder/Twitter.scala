package net.zyuiop.philofinder

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.{TwitterRestClient, TwitterStreamingClient}
import com.typesafe.scalalogging.LazyLogging
import net.zyuiop.philofinder.ShortestPathFinder.{Status, functionnalBfs}
import net.zyuiop.philofinder.Twitter.ComputedPath

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn
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
  val publicQueue: mutable.Queue[String] = mutable.Queue()
  val privateQueue: mutable.Queue[(Article, Tweet)] = mutable.Queue()
  val ex: ScheduledExecutorService = Executors.newScheduledThreadPool(6)
  var lastTweet: Long = 0
  var lastDmCheck: Long = 0
  var tweeting: Boolean = false

  def mainLoop(): Unit = {
    load()

    // Fills the queue with paths
    ex.scheduleAtFixedRate(() => computePublicPath(), 0, 1, TimeUnit.HOURS)

    // Replies to individual requests (2 threads to go faster)
    ex.scheduleAtFixedRate(() => computePrivatePath(), 0, 5, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => computePrivatePath(), 0, 5, TimeUnit.SECONDS)

    // Tweets a tweet every 15 minutes (runs more frequently in case of problem)
    ex.scheduleAtFixedRate(() => tweetNext(), 1, 1, TimeUnit.MINUTES)

    // Logs the status and saves
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
            val article = browser.searchRealArticle(tweetContent)
            privateQueue.enqueue((article, t))
          } catch {
            case e: Throwable =>
              e.printStackTrace()
              repeatIfFailing("reply not found " + t.id,
                client.createTweet("@" + t.user.get.screen_name + " La page '" + tweetContent + "' n'existe pas :(",
                  in_reply_to_status_id = Option.apply(t.id)))
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

  def repeatIfFailing[A](taskName: String, runnable: => Future[A], onSuccess: A => Unit = a => {},
                         onFailure: Throwable => Unit = a => {}, retryCnt: Int = 0): Unit = runnable.onComplete {
    case Failure(ex: Throwable) =>
      if (retryCnt >= 10) {
        logger.error(" !! Task " + taskName + " failed after 10 retries")
        onFailure(ex)
      } else {
        logger.error("  !! Failure on task " + taskName + ", retrying", ex)
        repeatIfFailing(taskName, runnable, onSuccess, onFailure, retryCnt + 1)
      }
    case Success(a) => onSuccess(a)
  }

  def logState(): Unit = {
    logger.info("----- STATE LOG ------")
    logger.info("Requests queue: " + privateQueue.length)
    logger.info("Tweets queue: " + publicQueue.length)
  }

  def buildPath(article: Article, consumer: String => Unit, onError: => Unit = () => ()): Unit = {
    val start = article
    try {
      val route = functionnalBfs(browser, target, Status(Queue(start), Map(start.url -> null.asInstanceOf[Article])))
      val tweet = buildTweet(ComputedPath(start, route))

      logger.info(" -> Generated tweet for route " + route)

      if (isTweetable(tweet)) consumer(tweet)
      else {
        logger.warn(" !! path not tweetable")
        onError
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        onError
    }
  }

  def computePrivatePath(): Unit = {
    if (privateQueue.nonEmpty) {
      logger.info("-> Computing a user path")
      val (start, t) = privateQueue.dequeue
      buildPath(start, result => {
        repeatIfFailing("reply content " + t.id,
          client.createTweet("@" + t.user.get.screen_name + " " + result, in_reply_to_status_id = Option.apply(t.id)))
      }, {
        repeatIfFailing("reply error " + t.id,
          client.createTweet("@" + t.user.get.screen_name + " Arf... Désolé, mais j'ai rencontré un problème en cherchant " +
            s"$target depuis ${start.name} :(", in_reply_to_status_id = Option.apply(t.id)))
      })
    }
  }

  def computePublicPath(): Unit = {
    if (publicQueue.lengthCompare(25) < 0) {
      logger.info("-> Computing a random path; current queue length: " + publicQueue.length)
      val start = browser.getRealArticle(default)
      buildPath(start, e => publicQueue.enqueue(e))

      computePublicPath() // Calls itself until queue is full
    }
  }

  def isTweetable(tweet: String): Boolean = tweet.length < 240

  def buildTweet(computedPath: ComputedPath): String = {
    def printRoute(hop: Int, list: List[Article], acc: String): String = {
      val hopName = if (hop == 0) "\uD83C\uDFE0" else "↳"

      if (list.isEmpty) acc
      else printRoute(hop + 1, list.tail, acc + "\n" + hopName + " " + list.head.name)
    }

    if (computedPath.path.isEmpty)
      s"${computedPath.source.name} vers $target :\n\nAucun chemin trouvé ! :o"
    else
      printRoute(0, computedPath.path, s"${computedPath.source.name} vers $target :\n") + "\n\nTotal : " + (computedPath.path.length - 1) + " pages"
  }

  def tweetNext(): Unit = {
    if (lastTweet + 900000 > System.currentTimeMillis)
      return
    logger.info("-> Trying to tweet")

    if (tweeting) {
      logger.warn(" !! An other tweet is being processed. Cancelling.")
      return
    }

    tweeting = true

    if (publicQueue.nonEmpty) {
      val t = publicQueue.dequeue()
      repeatIfFailing("automatic tweet", client.createTweet(t), a => {
        lastTweet = System.currentTimeMillis()
        tweeting = false
      }, e => {
        tweeting = false
      })
    } else {
      logger.error(" !! Nothing to tweet")
      tweeting = false
    }
  }

  def save(): Unit = {
    SavingManager.save[String]("tweets.tst", publicQueue, a => a)
  }

  def load(): Unit = {
    SavingManager.load[String]("tweets.tst", a => a).foreach(publicQueue.enqueue(_))
  }
}