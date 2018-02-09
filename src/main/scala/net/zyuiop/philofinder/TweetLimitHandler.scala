package net.zyuiop.philofinder

/**
  * @author Louis Vialar
  */
class TweetLimitHandler {
  var lastQuarterBegin: Long = 0
  var remainingTweets: Int = 24 // 25 - 1 (the automatic one)

  def tweet: Boolean = {
    if (lastQuarterBegin + 15*60*1000 < System.currentTimeMillis()) {
      lastQuarterBegin = System.currentTimeMillis()
      remainingTweets = 24
    }

    if (remainingTweets > 0) {
      remainingTweets -= 1
      true
    } else false
  }
}
