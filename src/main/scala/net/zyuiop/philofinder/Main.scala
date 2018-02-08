package net.zyuiop.philofinder

import net.zyuiop.philofinder.FirstLinkFinder.findFirstLinkPath
import net.zyuiop.philofinder.ShortestPathFinder.findShortestPath

/**
  * @author Louis Vialar
  */
object Main {
  def main(args: Array[String]): Unit = {
    val lang = Input.readInput("Wikipedia Language", "fr")
    val browser = new WikiBrowser(lang)

    run(browser)
  }

  def run(browser: WikiBrowser): Unit = {
    val page = Input.readInput("Wikipedia Page", "Spécial:Page_au_hasard")
    val searched = Input.readInput("Target Wikipedia Page", "Philosophie")
    val search = Input.readChoice("Type of search", Map("fls" -> "First Link Search", "sps" -> "Shortest Path Search"))

    if (search == "sps") findShortestPath(browser.getRealArticle(page), searched, browser)
    else findFirstLinkPath(Article(page, page), searched, browser)

    val retry = Input.readChoice("Do it again", Map("yes" -> "Run another search", "no" -> "Quit the script"))
    if (retry == "yes") run(browser)

  }

}
