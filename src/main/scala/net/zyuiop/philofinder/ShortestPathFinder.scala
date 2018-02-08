package net.zyuiop.philofinder

import scala.collection.mutable
import scala.collection.mutable.Queue

/**
  * @author Louis Vialar
  */
object ShortestPathFinder {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")
    val searched = Input.readInput("Target Wikipedia Page", "Philosophie")

    /*val searcher = new ShortestFinder(searched, browser)
    searcher.next(ShortestStatus(page, page, 0, Set()))*/

    bfs(Article(page, page), browser, searched)
  }

  /*class ShortestFinder(targetPage: String, browser: WikiBrowser) {
    def next(status: ShortestStatus): Unit = status match {
      case ShortestStatus(currentPage: String, currentPageTitle: String, hops: Int, previousPages: Set[String]) =>





        println(hops + ": " + currentPageTitle + " [[" + currentPage + "]]")

        if (currentPage.equalsIgnoreCase(targetPage) || currentPageTitle.equalsIgnoreCase(targetPage)) {
          println("==> Found target page " + targetPage + " in " + hops + " hops!")
        } else if (status.previousPages(currentPage)) {
          println("==> Found loop on page " + currentPage + " after " + hops + " hops!")
        } else {
          val nextPage = browser.getFirstLink(currentPage)

          if (nextPage == null) {
            println("==> End: no link found")
          } else {
            next(ShortestStatus(nextPage._1, nextPage._2, hops + 1, previousPages + currentPage))
          }
        }
    }

  }

  case class ShortestStatus(currentPage: String, currentPageTitle: String, hops: Int, previousPages: Set[String])
  case class PotentialPath(currentPage: Article, previousPages: List[Article])
*/
  def bfs(start: Article, browser: WikiBrowser, target: String) = {
    var q: mutable.Queue[Article] = mutable.Queue()
    var parents: mutable.Map[String, (Article, Int)] = mutable.Map() // parents on the shortest path

    val real = browser.getRealArticle(start.url)

    parents.put(real.url, (null, 0))
    q.enqueue(real)

    println("Starting from " + real.name)

    var cont = true
    var targetArticle: Article = null

    while (q.nonEmpty && cont) {
      val elem = q.dequeue
      val depth = parents(elem.url)._2

      val links = browser.getLinks(elem.url)

      println("-----" + elem + "-" + depth + "------")
      links.foreach(println)

        links
        .filter(art => !parents.keySet(art.url))
        .foreach(art => {
            if (cont) {
              q.enqueue(art)

              if (art.name.equalsIgnoreCase(target) || art.url.equalsIgnoreCase(target)) {
                cont = false
                targetArticle = art
              }

              parents.put(art.url, (elem, depth + 1))
            }
          })
    }

    val route = buildRoute(parents, targetArticle).reverse

    println("Chemin trouvé en " + (route.length - 1) + " clics")

    printRoute(0, route)
  }

  def printRoute(hop: Int, list: List[Article]): Unit = {
    if (list.isEmpty) return
    println(hop + ": " + list.head.name + " [[" + list.head.url + "]]")
    printRoute(hop + 1, list.tail)
  }

  def buildRoute(parents: mutable.Map[String, (Article, Int)], current: Article): List[Article] = {
    val parent = parents(current.url)._1
    if (parent == null) current :: Nil
    else current :: buildRoute(parents, parent)
  }

  /*
  ParcoursLargeur(Graphe G, Sommet s):
       f = CreerFile();
       f.enfiler(s);
       marquer(s);
       tant que la file est non vide
                s = f.defiler();
                afficher(s);
                pour tout voisin t de s dans G
                         si t non marqué
                                 f.enfiler(t);
                                 marquer(t);
   */
}



