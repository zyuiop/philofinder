package net.zyuiop.philofinder

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elements}

class WikiBrowser(lang: String) {
  val browser = JsoupBrowser()

  def getUrl(name: String): String = "https://" + lang + ".wikipedia.org/wiki/" + name

  def getRealArticle(name: String): Article = {
    val page = browser.get(getUrl(name))
      Article(page.title.split(" — ")(0), page.location.replace(getUrl(""), ""))
  }


  def getLinkElements(name: String): Iterable[Element] = {
    (browser.get(getUrl(name)) >> element("#mw-content-text"))
      .children.head.children
      .filter(_.tagName == "p")
      .flatMap(_ >> elements("a:not(.new):not(.selflink):not(.internal)"))
      .filter(_.hasAttr("href"))
      .filter(_.hasAttr("title"))
      .filterNot(_.attr("href").startsWith("#"))
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