package net.zyuiop.philofinder

import com.typesafe.scalalogging.LazyLogging

import scala.io.Source
import scala.reflect.io.File

/**
  * @author Louis Vialar
  */
object SavingManager extends LazyLogging {
  val saveSeparator = "<!!TWEETS_SEPARATOR!!>"

  def save[A](path: String, data: Seq[A], transformer: A => String): Unit = {
    logger.info(s"Saving $path...")
    File(path).writeAll(data.map(transformer).mkString(saveSeparator))
  }

  def load[A](path: String, transformer: String => A): Seq[A] = {
    logger.info(s"Loading $path...")
    val file = File(path)
    if (file.exists) {
      val data = Source.fromFile(path)
      if (data.nonEmpty)
        data.mkString.split(saveSeparator).map(transformer)
      else Nil
    } else Nil
  }
}
