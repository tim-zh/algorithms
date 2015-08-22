import org.scalatest.{FlatSpec, Matchers}

class MaxSubsequenceSpec extends FlatSpec with Matchers {

	"MaxSubsequence" should "find max subsequence" in {
		val seq = Seq(-1, -100, 1, -3, 2, 100, -2, 10, -9, 2)

		val result = MaxSubsequence.find(seq)

		result should be(110)
	}

	it should "find(Kadane) max subsequence" in {
		val seq = Seq(-1, -100, 1, -3, 2, 100, -2, 10, -9, 2)

		val result = MaxSubsequence.findKadane(seq)

		result should be(110)
	}
}
