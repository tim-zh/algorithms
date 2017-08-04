import org.scalatest.{FlatSpec, Matchers}

class TravelingSalesmanSpec extends FlatSpec with Matchers {
  "TravelingSalesman" should "find the shortest path" in {
    val node1 = Array(0d, 0d)
    val node2 = Array(10d, 0d)
    val node3 = Array(0d, 10d)
    val node4 = Array(10d, 10d)
    val node5 = Array(5d, 5d)

    val result = TravelingSalesman.findPath(Set(node1, node2, node3, node4, node5))

    result._1 should be < 35d
    result._2 should contain(node1)
    result._2 should contain(node2)
    result._2 should contain(node3)
    result._2 should contain(node4)
    result._2 should contain(node5)
  }
}
