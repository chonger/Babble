import twitter4j.conf.ConfigurationBuilder
import twitter4j.TwitterFactory

object Tweeter {

  def main(args : Array[String]) = {

    val cb = new ConfigurationBuilder();
    cb.setDebugEnabled(true)
    .setOAuthConsumerKey("43kGPwdMkktTsBTFX6YFjtESI")
    .setOAuthConsumerSecret("an2GBrSQpBBc0Cm395YczgfvfmxIrootWNhFRotTIeRRRRwFI2")
    .setOAuthAccessToken("39544905-i4hM42aIQHKnZXCbTMUiaCDxdD2N5IJuudLIU1s6a")
    .setOAuthAccessTokenSecret("ZlDIO3tYtjZjtDyKnC6Tys6oQ3EfyozRSF6jWgqao4HvT")
    val tf = new TwitterFactory(cb.build())
    val twitter = tf.getInstance()

    twitter.updateStatus("100010101011001010111")

    println(twitter) 

  }

}
