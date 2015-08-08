import org.scalatest.{FlatSpec, Matchers}


class BinarySearchSpec extends FlatSpec with Matchers {

  "BinarySearch" should "search" in {
    val xs = Array[Integer](1, 2, 3, 6, 8, 12)

    BinarySearch.searchIndex[Integer](2, xs) should be(1)
    BinarySearch.searchIndex[Integer](12, xs) should be(5)
    BinarySearch.searchIndex[Integer](4, xs) should be(-1)
  }
}
