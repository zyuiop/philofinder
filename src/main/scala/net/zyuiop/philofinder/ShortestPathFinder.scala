package net.zyuiop.philofinder

import scala.collection.immutable.Queue


/**
  * @author Louis Vialar
  */
object ShortestPathFinder {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")
    val searched = Input.readInput("Target Wikipedia Page", "Philosophie")

    printShortestPath(browser.getRealArticle(page), searched, browser)
  }

  def printShortestPath(start: Article, target: String, browser: WikiBrowser): Unit = {
    println("Starting from " + start.name)
    val route = functionnalBfs(browser, target, Status(Queue(start), Map(start.url -> null.asInstanceOf[Article])), verbose = true)
    println()
    println("Chemin trouvé en " + (route.length - 1) + " clics")
    printRoute(0, route)
  }


  case class Status(queue: Queue[Article], parents: Map[String, Article])

  class FoldResult
  case class NotFoundFoldResult(queue: Queue[Article], parents: Map[String, Article]) extends FoldResult
  case class FoundFoldResult(parents: Map[String, Article], foundTarget: Article = null) extends FoldResult

  def functionnalBfs(browser: WikiBrowser, target: String, status: Status, verbose: Boolean = false): List[Article] = status match {
    case Status(queue, parents) =>
      if (queue.isEmpty) {
        if (verbose)
          println("Hitler not found!")
        return List()
      }

      val deq = queue.dequeue
      val elem = deq._1
      val nQueue = deq._2
      val links = browser.getLinks(elem.url)

      if (verbose)
        println(" ... Finding links for page " + elem.name)

      val result = links.toSet
        .filter(art => !parents.keySet(art.url))
        .filterNot(_.name.startsWith("Wikipédia:"))
        .filterNot(_.name.startsWith("Spécial:"))
        .filterNot(_.url.startsWith("Portail:"))
        .foldLeft(NotFoundFoldResult(nQueue, parents).asInstanceOf[FoldResult])((out, art) => out match {
          case FoundFoldResult(_, _) => out
          case NotFoundFoldResult(q, p) =>
            if (art.name.equalsIgnoreCase(target) || art.url.equalsIgnoreCase(target)) {
              // If we have the target we return a Found result
              FoundFoldResult(parents + (art.url -> elem), art)
            } else {
              // We just enqueue the current article and add its parents to the map
              NotFoundFoldResult(q.enqueue(art), p + (art.url -> elem))
            }
        })

      result match {
        case NotFoundFoldResult(q, p) => functionnalBfs(browser, target, Status(q, p), verbose)
        case FoundFoldResult(p, article) => buildRoute(p, article).reverse
      }
  }

  def buildRoute(parents: Map[String, Article], current: Article): List[Article] = {
    val parent = parents(current.url)
    if (parent == null) current :: Nil
    else current :: buildRoute(parents, parent)
  }

  def printRoute(hop: Int, list: List[Article]): Unit = {
    if (list.isEmpty) return
    println(hop + ": " + list.head.name + " [[" + list.head.url + "]]")
    printRoute(hop + 1, list.tail)
  }
}



