package net.zyuiop.philofinder

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elements}

import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object PhiloFinder {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")
    val searched = Input.readInput("Target Wikipedia Page", "Philosophie")

    val searcher = new Searcher(searched, browser)
    searcher.next(Status(page, page, 0, Set()))
  }

  class WikiBrowser(lang: String) {
    val browser = JsoupBrowser()

    def getUrl(name: String) = "https://" + lang + ".wikipedia.org/wiki/" + name

    def getFirstLink(name: String): (String, String) = {
      val url = getUrl(name)
      val content = (browser.get(url) >> element("#mw-content-text")).children.head
      val links = content.children.filter(_.tagName == "p") flatMap
        (_ >> elements("a:not(.new):not(.selflink):not(.internal)")) filter (_.hasAttr("href")) filter (_.hasAttr("title")) filterNot (_.attr("href").startsWith("#"))

      if (links.isEmpty) null
      else {
        val array: Array[String] = links.head.attr("href").split("/")
        (array(array.length - 1), links.head.attr("title"))
      }
    }
  }

  object Input {
    def readInput(prompt: String, default: String): String = {
      print(prompt + " [" + default + "]: ")
      val res = StdIn.readLine()

      if (res.isEmpty) default
      else res
    }
  }

  class Searcher(targetPage: String, browser: WikiBrowser) {
    def next(status: Status): Unit = status match {
      case Status(currentPage: String, currentPageTitle: String, hops: Int, previousPages: Set[String]) =>
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
            next(Status(nextPage._1, nextPage._2, hops + 1, previousPages + currentPage))
          }
        }
    }

  }

  case class Status(currentPage: String, currentPageTitle: String, hops: Int, previousPages: Set[String])

}



