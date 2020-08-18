package net.zyuiop.philofinder.helpers

import com.typesafe.scalalogging.Logger

/**
  * @author Louis Vialar
  */
class TweetLimitHandler {
  var lastQuarterBegin: Long = 0
  var remainingTweets: Int = 63 // 50 - 2 (the automatic one) + 15 (we don't tweet a lot all the day)

  def tweet(): Boolean = {
    if (lastQuarterBegin + 30*60*1000 < System.currentTimeMillis()) {
      lastQuarterBegin = System.currentTimeMillis()
      remainingTweets = 63
    }

    remainingTweets -= 1
    remainingTweets >= 0
  }

  def printStatus(logger: Logger): Unit = {
    logger.info("Remaining tweet: " + remainingTweets)
    logger.info("Quarter begin: " + lastQuarterBegin)
  }
}
