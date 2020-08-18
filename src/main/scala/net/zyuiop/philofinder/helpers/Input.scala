package net.zyuiop.philofinder.helpers

import scala.io.StdIn

object Input {
  def readInput(prompt: String, default: String): String = {
    print(prompt + " [" + default + "]: ")
    val res = StdIn.readLine()

    if (res.isEmpty) default
    else res
  }

  def readChoice(prompt: String, choices: Map[String, String]): String = {
    println(prompt + ": ")
    choices.foreach(entry => println(entry._1 + ": " + entry._2))
    print("Choice? ")
    val res = StdIn.readLine()

    if (choices.keySet.map(_.toLowerCase).contains(res.toLowerCase)) res.toLowerCase
    else readChoice(prompt, choices)
  }
}
