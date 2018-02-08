package net.zyuiop.philofinder

import scala.io.StdIn

object Input {
  def readInput(prompt: String, default: String): String = {
    print(prompt + " [" + default + "]: ")
    val res = StdIn.readLine()

    if (res.isEmpty) default
    else res
  }
}