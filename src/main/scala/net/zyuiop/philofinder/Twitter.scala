package net.zyuiop.philofinder

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.entities.enums.WithFilter
import com.danielasfregola.twitter4s.http.clients.streaming.TwitterStream
import com.danielasfregola.twitter4s.{TwitterRestClient, TwitterStreamingClient}
import com.typesafe.scalalogging.LazyLogging
import net.zyuiop.philofinder.ShortestPathFinder.{Status, functionnalBfs}
import net.zyuiop.philofinder.Twitter.ComputedPath
import net.zyuiop.philofinder.helpers.{PersistenceHelper, TweetLimitHandler}

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

  case class ComputedPath(source: Article, target: String, path: List[Article])

  case class State(readyUser: Queue[ComputedPath], readyAutomatic: Queue[ComputedPath])

}

class Twitter(browser: WikiBrowser, client: TwitterRestClient, streaming: TwitterStreamingClient, target: String, default: String, username: String) extends LazyLogging {
  val publicQueue: mutable.Queue[String] = mutable.Queue()
  val privateQueue: mutable.Queue[(Article, Tweet, String)] = mutable.Queue()
  val waitingReplies: mutable.Queue[(String, Tweet)] = mutable.Queue()
  val ex: ScheduledExecutorService = Executors.newScheduledThreadPool(7)
  var lastTweet: Long = 0
  var lastDmCheck: Long = 0
  var tweeting: Boolean = false
  val limits = new TweetLimitHandler

  def mainLoop(): Unit = {
    load()


    // Fills the queue with paths
    ex.scheduleAtFixedRate(() => computePublicPath(), 0, 1, TimeUnit.HOURS)

    // Replies to individual requests (2 threads to go faster)
    ex.scheduleAtFixedRate(() => computePrivatePath(), 0, 5, TimeUnit.SECONDS)
    ex.scheduleAtFixedRate(() => computePrivatePath(), 0, 5, TimeUnit.SECONDS)

    // Tweets replies to individual requests (every 25 seconds)
    ex.scheduleAtFixedRate(() => tweetPrivatePath(), 0, 25, TimeUnit.SECONDS)

    // Tweets a tweet every 15 minutes (runs more frequently in case of problem)
    ex.scheduleAtFixedRate(() => tweetNext(), 1, 1, TimeUnit.MINUTES)

    // Logs the status and saves
    ex.scheduleAtFixedRate(() => logState(), 0, 1, TimeUnit.MINUTES)
    ex.scheduleAtFixedRate(() => save(), 0, 1, TimeUnit.MINUTES)

    logger.info("Started twitter bot with target " + target)

    val stream = openStream()

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

  def openStream(): Future[TwitterStream] = {
    println("Opening stream...")

    streaming.filterStatuses(tracks = Seq(s"@$username"))({
      case t: Tweet =>
        if (t.in_reply_to_screen_name.getOrElse("").equalsIgnoreCase(username)
          && t.in_reply_to_status_id.isEmpty
          && t.in_reply_to_status_id_str.isEmpty
          && t.user.isDefined
          && !t.user.get.screen_name.equalsIgnoreCase(username)) {

          val tweetContent = t.text.replaceAll("@[a-zA-Z0-9_-]+", "").trim
          logger.info(s" > extracted page $tweetContent")

          val future = if (tweetContent.contains("==&gt;")) {
            val parts = tweetContent.split("==&gt;").map(_.trim)
            Future {
              val src = browser.searchRealArticle(parts(0))
              val target = browser.searchRealArticle(parts(1)).name

              (src, target)
            }
          } else {
            Future {
              (browser.searchRealArticle(tweetContent), target)
            }
          }

          future.recover {
            case e: Throwable =>
              e.printStackTrace()
              repeatIfFailing("fav not found " + t.id, client.favoriteStatus(t.id))
              throw e // The re-thrown ex will be lost
          } foreach {
            case (src, target) =>
              if (src.name.startsWith("Spécial:") || src.name.startsWith("Wikipédia:") || target.startsWith("Spécial:") || target.startsWith("Wikipédia:")) {
                repeatIfFailing("fav illegal " + t.id, client.favoriteStatus(t.id))
                logger.info(s" > illegal page $src or $target")
              } else {
                privateQueue.enqueue((src, t, target))
                logger.info(s" > queued page $src => $target")
              }
          }
        }
    }).recoverWith {
      case e: Throwable =>
        logger.error("Listener crashed", e)
        openStream()
    }
  }

  def repeatIfFailing[A](taskName: String, runnable: => Future[A], onSuccess: => Unit = {},
                         onFailure: => Unit = {}, retryCnt: Int = 0): Unit = runnable.onComplete {
    case Failure(ex: Throwable) =>
      if (retryCnt >= 10) {
        logger.error(" !! Task " + taskName + " failed after 10 retries")
        onFailure
      } else {
        logger.error("  !! Failure on task " + taskName + ", retrying", ex)
        repeatIfFailing(taskName, runnable, onSuccess, onFailure, retryCnt + 1)
      }
    case Success(a) => onSuccess
  }

  def logState(): Unit = {
    logger.info("----- STATE LOG ------")
    logger.info("Requests queue: " + privateQueue.length)
    logger.info("Tweets queue: " + publicQueue.length)
    logger.info("Replies queue: " + waitingReplies.length)
    limits.printStatus(logger)
    logger.info("----- STATE LOG ------")
  }

  def buildPath(article: Article, target: String, consumer: String => Unit, onError: => Unit = () => ()): Unit = {
    val start = article
    try {
      val route = functionnalBfs(browser, target, Status(Queue(start), Map(start.url -> null.asInstanceOf[Article])))
      val tweet = buildTweet(ComputedPath(start, target, route))

      logger.info(" -> Generated tweet for route " + route)

      if (isTweetable(tweet)) consumer(tweet)
      else {
        consumer("Chemin depuis " + article.name + " trop long pour être tweeté :o")
        logger.warn(" !! path not tweetable")
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        onError
    }
  }

  def computePrivatePath(): Unit = {
    if (privateQueue.nonEmpty) {
      val (start, t, target) = privateQueue.dequeue
      logger.info(s"-> Computing a user path from $start")
      buildPath(start, target, result => {
        waitingReplies.enqueue((result, t))

        if (waitingReplies.lengthCompare(5) > 0)
          repeatIfFailing("dm delay " + t.id, client.createDirectMessageEvent(t.user.get.id, "J'ai bien calculé un" +
            s" chemin depuis ${start.name}, mais j'ai actuellement beaucoup de demandes en attente. Votre réponse sera" +
            s" tweetée dans ${waitingReplies.length * 25} secondes environ."))
      }, {
        repeatIfFailing("fav error " + t.id, client.favoriteStatus(t.id))
      })
    }
  }

  def tweetPrivatePath(): Unit = if (waitingReplies.nonEmpty) {
    if (!limits.tweet) {
      logger.info("-> TweetLimit - slowing down")
      return
    }

    logger.info("-> Tweeting a user path")
    val (tweet, t) = waitingReplies.dequeue
    repeatIfFailing("reply content " + t.id, client.createTweet("@" + t.user.get.screen_name + " " + tweet,
      in_reply_to_status_id = Option.apply(t.id)))
  }

  def computePublicPath(): Unit = {
    if (publicQueue.lengthCompare(25) < 0) {
      logger.info("-> Computing a random path; current queue length: " + publicQueue.length)
      val start = browser.getRealArticle(default)
      buildPath(start, target, e => publicQueue.enqueue(e))

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
      printRoute(0, computedPath.path, s"${computedPath.source.name} vers ${computedPath.target} :\n") + "\n\nTotal : " + (computedPath.path.length - 1) + " pages"
  }

  def tweetNext(): Unit = {
    if (lastTweet + 900000 > System.currentTimeMillis)
      return
    logger.info("-> Trying to tweet")


    if (publicQueue.nonEmpty) {
      val t = publicQueue.dequeue()
      repeatIfFailing("automatic tweet", client.createTweet(t), {
        lastTweet = System.currentTimeMillis()
      }, {
        logger.error("Tweeting failed forever. I will not retry.")
      })
    } else {
      logger.error(" !! Nothing to tweet")
      tweeting = false
    }
  }

  def save(): Unit = {
    PersistenceHelper.save[String]("tweets.tst", publicQueue, a => a)
  }

  def load(): Unit = {
    PersistenceHelper.load[String]("tweets.tst", a => a).foreach(publicQueue.enqueue(_))
  }
}