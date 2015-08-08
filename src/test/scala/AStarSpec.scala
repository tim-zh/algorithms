import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer


class AStarSpec extends FlatSpec with Matchers {
  
  "AStar" should "find a path" in {
    val start = TestNode(0, 0)
    val node1 = TestNode(1, 1)
    val node2 = TestNode(1, 0)
    val end = TestNode(2, 0)
    start._neighbors ++= Seq(node1, node2)
    node1._neighbors ++= Seq(node2, end)
    node2._neighbors ++= Seq(node1, end)

    val path = AStar.getPath(start, end)

    path.get should contain(start)
    path.get should contain(node2)
    path.get should contain(end)
    path.get should not contain node1
  }

  case class TestNode(x: Int, y: Int) extends AStar.Node[TestNode] {

    val _neighbors = ArrayBuffer[TestNode]()

    override def distanceTo(n: TestNode): Double = Math.hypot(x - n.x, y - n.y)

    override def neighbors: Iterable[TestNode] = _neighbors
  }
}
