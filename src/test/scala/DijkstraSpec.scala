import org.scalatest.{FlatSpec, Matchers}

class DijkstraSpec extends FlatSpec with Matchers {
  "Dijkstra" should "calculate absolute paths" in {
    val n1 = new TestNode
    val n2 = new TestNode
    val n3 = new TestNode
    val n4 = new TestNode
    n1._neighbors = Map[Dijkstra.Node, Double](n2 -> 14, n3 -> 10, n4 -> 7)
    n2._neighbors = Map[Dijkstra.Node, Double](n1 -> 1, n3 -> 2)
    n3._neighbors = Map[Dijkstra.Node, Double](n2 -> 2, n1 -> 1, n4 -> 2)
    n4._neighbors = Map[Dijkstra.Node, Double](n1 -> 7, n3 -> 2)

    val weights = Dijkstra.calculatePathWeights(n1)

    weights(n1) should be(0)
    weights(n2) should be(11)
    weights(n3) should be(9)
    weights(n4) should be(7)
  }

  class TestNode extends Dijkstra.Node {
    var _neighbors: Map[Dijkstra.Node, Double] = null

    override def neighbors: Iterable[Dijkstra.Node] = _neighbors.keySet

    override def weightOf(neighbor: Dijkstra.Node): Double = _neighbors(neighbor)
  }

}
