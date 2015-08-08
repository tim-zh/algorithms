import org.scalatest.{FlatSpec, Matchers}


class QuickSortSpec extends FlatSpec with Matchers {

  "QuickSort" should "sort integers" in {
    val seq = Array(29, 25, 3, 49, 9, 37, 21, 43)

    val result = QuickSort.sort(seq)

    result(0) should be(3)
    result(1) should be(9)
    result(2) should be(21)
    result(3) should be(25)
    result(4) should be(29)
    result(5) should be(37)
    result(6) should be(43)
    result(7) should be(49)
  }

  it should "sort integers in-place" in {
    val seq = Array(29, 25, 3, 49, 9, 37, 21, 43)

    QuickSort.sortInPlace(seq)

    seq(0) should be(3)
    seq(1) should be(9)
    seq(2) should be(21)
    seq(3) should be(25)
    seq(4) should be(29)
    seq(5) should be(37)
    seq(6) should be(43)
    seq(7) should be(49)
  }

  it should "sort edge case collections in-place" in {
    val seq1 = Array(29)
    val seq2 = Array(29, 25)
    val seq3 = Array(25, 29, 3)

    QuickSort.sortInPlace(seq1)
    QuickSort.sortInPlace(seq2)
    QuickSort.sortInPlace(seq3)

    seq1(0) should be(29)

    seq2(0) should be(25)
    seq2(1) should be(29)

    seq3(0) should be(3)
    seq3(1) should be(25)
    seq3(2) should be(29)
  }
}
