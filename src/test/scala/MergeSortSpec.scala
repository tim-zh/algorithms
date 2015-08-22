import org.scalatest.{FlatSpec, Matchers}


class MergeSortSpec extends FlatSpec with Matchers {

	private def test(mergeStrategy: (Seq[Int], Seq[Int]) => Seq[Int]) {
		val seq = Seq(29, 25, 3, 49, 9, 37, 21, 43)

		val result = MergeSort.sort(seq, mergeStrategy)

		result(0) should be(3)
		result(1) should be(9)
		result(2) should be(21)
		result(3) should be(25)
		result(4) should be(29)
		result(5) should be(37)
		result(6) should be(43)
		result(7) should be(49)
	}

	private def testEdgeCase(mergeStrategy: (Seq[Int], Seq[Int]) => Seq[Int]) {
		val seq1 = Seq(29)
		val seq2 = Seq(29, 25)

		val result1 = MergeSort.sort(seq1, mergeStrategy)
		val result2 = MergeSort.sort(seq2, mergeStrategy)

		result1(0) should be(29)
		result2(0) should be(25)
		result2(1) should be(29)
	}

	"MergeSort" should "sort integers" in {
		test(MergeSort.merge)
	}

	it should "sort edge case collections" in {
		testEdgeCase(MergeSort.merge)
	}

	it should "sort(tailrec merge) integers" in {
		test(MergeSort.mergeTailRec(_, _))
	}

	it should "sort(tailrec merge) edge case collections" in {
		testEdgeCase(MergeSort.mergeTailRec(_, _))
	}

	it should "sort(nonrec merge) integers" in {
		test(MergeSort.mergeNonRec)
	}

	it should "sort(nonrec merge) edge case collections" in {
		testEdgeCase(MergeSort.mergeNonRec)
	}
}
