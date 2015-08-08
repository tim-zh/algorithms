import org.scalatest.{FlatSpec, Matchers}


class BucketSortSpec extends FlatSpec with Matchers {

  "BucketSort" should "sort integers" in {
    val seq = Seq(29, 25, 3, 49, 9, 37, 21, 43)

    val result = BucketSort.sort(seq)

    result(0) should be(3)
    result(1) should be(9)
    result(2) should be(21)
    result(3) should be(25)
    result(4) should be(29)
    result(5) should be(37)
    result(6) should be(43)
    result(7) should be(49)
  }

  it should "sort edge case collections" in {
    val seq1 = Seq(29)
    val seq2 = Seq(29, 25)

    val result1 = BucketSort.sort(seq1)
    val result2 = BucketSort.sort(seq2)

    result1(0) should be(29)
    result2(0) should be(25)
    result2(1) should be(29)
  }
}
