import org.scalatest.{FlatSpec, Matchers}

class BreadthSearchSpec extends FlatSpec with Matchers {
	"BreadthSearch" should "search for node in a graph" in {
		val node1 = TestNode(1)
		val node2 = TestNode(2)
		val node3 = TestNode(3)
		val node4 = TestNode(4)
		val node5 = TestNode(5)
		val node6 = TestNode(6)
		node1._neighbors ++= Set(node2, node3)
		node2._neighbors += node4
		node4._neighbors ++= Set(node1, node5)
		node5._neighbors += node1
		node6._neighbors ++= Set(node2, node3)

		BreadthSearch.search(node5, node1) should be(true)
		BreadthSearch.search(node6, node1) should be(false)
	}

	case class TestNode(id: Int) extends BreadthSearch.Node {
		var _neighbors = Set[TestNode]()

		override def neighbors: Iterable[BreadthSearch.Node] = _neighbors
	}

}
