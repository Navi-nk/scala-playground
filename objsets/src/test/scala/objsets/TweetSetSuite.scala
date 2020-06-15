package objsets

import objsets.GoogleVsApple._
import objsets.TweetReader.toTweetSet
import org.junit._
import org.junit.Assert.assertEquals

class TweetSetSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  @Test def `filter: on empty set`: Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
    }

  @Test def `filter: a on set5`: Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: twenty on set5`: Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `union: set4c and set4d`: Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set2`: Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `descending: set5`: Unit =
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }

  @Test def `Filter test: custom` : Unit = {
    new TestSets {
      val tweets = TweetReader.tweetMap("mashable") ++ TweetReader.tweetMap("gizmodo")
      val result = toTweetSet(tweets).filter(tw => tw.retweets == 321 || tw.retweets == 205)
      assertEquals(2 , size(result))
    }
  }

  @Test def `descending test: custom` : Unit = {
    new TestSets {
      val tweets = TweetReader.tweetMap("mashable") ++ TweetReader.tweetMap("gizmodo")
      val result = toTweetSet(tweets).filter(tw => tw.retweets == 321 || tw.retweets == 205).descendingByRetweet
      result.foreach(t => println(t.retweets))
      assertEquals(321 ,result.head.retweets)

      GoogleVsApple.trending//.foreach(t => println(t.retweets))
      //println("gap")
      //(googleTweets union appleTweets)
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
