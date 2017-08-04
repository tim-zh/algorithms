import org.scalatest.{FlatSpec, Matchers}

class BloomFilterSpec extends FlatSpec with Matchers {
  "BloomFilter" should "probably check stored elements" in {
    val filter = new BloomFilter[String](10)
    filter.add("123")
    filter.add("abc")

    filter("123") should be(true)
    filter("abc") should be(true)
    filter("hay") should be(false)
  }
}
