package net.zyuiop.philofinder.cli

import net.zyuiop.philofinder.{Article, WikiBrowser}

/**
  * @author Louis Vialar
  */
object FirstLinkFinder {
  def findFirstLinkPath(start: Article, target: String, browser: WikiBrowser): Unit = {
    val searcher = new Searcher(target, browser)
    searcher.next(Status(start, 0, Set()))
  }

  class Searcher(targetPage: String, browser: WikiBrowser) {
    def next(status: Status): Unit = status match {
      case Status(Article(currentPage: String, currentPageTitle: String), hops: Int, previousPages: Set[String]) =>
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
            next(Status(nextPage, hops + 1, previousPages + currentPage))
          }
        }
    }

  }

  case class Status(currentPage: Article, hops: Int, previousPages: Set[String])
}
