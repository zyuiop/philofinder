package net.zyuiop.philofinder

/**
  * @author Louis Vialar
  */
object FirstLinkFinder {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")
    val searched = Input.readInput("Target Wikipedia Page", "Philosophie")

    val searcher = new Searcher(searched, browser)
    searcher.next(Status(Article(page, page), 0, Set()))
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



