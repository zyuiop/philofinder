package net.zyuiop.philofinder.cli

import net.zyuiop.philofinder.helpers.Input
import net.zyuiop.philofinder.{Article, WikiBrowser}

import scala.collection.immutable.Queue
import scala.reflect.io.File

/**
  * @author Louis Vialar
  */
object GraphExplorer {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")

    buildGraph(browser.getRealArticle(page), browser)
  }

  def buildGraph(start: Article, browser: WikiBrowser): Unit = {
    println("Starting from " + start.name)
    val startLink = LinkedArticle(start, null, 0)
    val map = functionnalBfs(browser, Status(Queue(startLink), Map(start.url -> startLink)))
    println()
    saveMap(map, start.name)
    println(map)
  }

  case class Status(queue: Queue[LinkedArticle], parents: Map[String, LinkedArticle])

  case class LinkedArticle(article: Article, parent: LinkedArticle, depth: Int)

  @scala.annotation.tailrec
  def functionnalBfs(browser: WikiBrowser, status: Status): Map[String, LinkedArticle] = status match {
    case Status(queue, parents) =>
      if (queue.isEmpty) {
        println("Finished!")
        return parents
      }

      val (elem, nQueue) = queue.dequeue
      val links = browser.getLinks(elem.article.url)

      println(" ... Finding links for page " + elem.article.name + " (depth: " + elem.depth + ")")

      val (nq, np) = links.toSet.filter(art => !parents.keySet(art.url))
        .foldLeft((nQueue, parents))((out, art) => out match {
          case (q, p) =>
            val linked = LinkedArticle(art, elem, elem.depth + 1)
            (q.enqueue(linked), p + (art.url -> linked))
        })

      functionnalBfs(browser, Status(nq, np))
  }

  def saveMap(map: Map[String, LinkedArticle], name: String): Unit = {
    File(name + "_graph.txt").writeAll(map.toList.map(el => el._1 + " ====> " + el._2.parent.article.url).mkString("\n"))
  }
}
