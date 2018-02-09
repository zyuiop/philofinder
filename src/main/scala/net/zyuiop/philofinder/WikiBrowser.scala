package net.zyuiop.philofinder

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elements}
import org.jsoup.HttpStatusException

class WikiBrowser(lang: String) {
  val browser = JsoupBrowser()

  def getUrl(name: String): String = "https://" + lang + ".wikipedia.org/wiki/" + name
  def getSearchUrl(name: String): String = "https://" + lang + ".wikipedia.org/w/index.php?search=" + name

  def getRealArticle(name: String): Article = {
    val page = browser.get(getUrl(name))
      Article(page.location.replace(getUrl(""), ""), page.title.split(" — ")(0))
  }

  def searchRealArticle(name: String): Article = {
    val page = browser.get(getSearchUrl(name))

    if (!page.location.startsWith(getUrl("")))
      throw new Exception("Article not found")

    Article(page.location.replace(getUrl(""), ""), page.title.split(" — ")(0))
  }

  def getLinkElements(name: String): Iterable[Element] = {
    try {
      (browser.get(getUrl(name)) >> element("#mw-content-text"))
        .children.head.children
        .filter(el => el.tagName == "p" || el.tagName == "ul")
        .flatMap(_ >> elements("a:not(.new):not(.selflink):not(.internal):not(.extiw)"))
        .filter(_.hasAttr("href"))
        .filter(_.hasAttr("title"))
        .filterNot(_.attr("href").startsWith("#"))
    } catch {
      case _: HttpStatusException => List()
    }
  }

  def getLinks(name: String): Iterable[Article] =
    getLinkElements(name).map(link => {
      val array: Array[String] = link.attr("href").split("/")
      Article(array(array.length - 1), link.attr("title"))
    })

  def getFirstLink(name: String): Article = {
    val links = getLinks(name)
    if (links.isEmpty) null
    else links.head
  }
}

case class Article(url: String, name: String)