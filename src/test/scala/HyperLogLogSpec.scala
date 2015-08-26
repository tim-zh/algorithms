import org.scalatest.{FlatSpec, Matchers}


class HyperLogLogSpec extends FlatSpec with Matchers {
	"HyperLogLog" should "calculate an approximate number of collection's unique elements" in {
		val xs = Seq(-330651821, -722209082, 313622883, 641200391, -1399691884, 1696384529, -1135661078, 1716032436,
			1900377650, -1745156164, -773252329, 1062142571, 978433737, -524856714, -21689243, -301236471, 1389570273,
			102160159, -796891862, -1121238341, 313622883, 313622883, -726582845, -868285096, -1121238341, -35349108,
			1911067300, 397757780, 1013116707, -1283654849, -1000418655, -1135661078)
		val uniques = 28

		val result = HyperLogLog.getApproximateSetCardinality(xs, 3)

		(math.abs(result - uniques) / uniques.asInstanceOf[Double]) should be < 0.02
	}
}
