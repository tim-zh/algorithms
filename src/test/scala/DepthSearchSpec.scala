import org.scalatest.{FlatSpec, Matchers}

class DepthSearchSpec extends FlatSpec with Matchers {
	private def test(search: (DepthSearch.Node, DepthSearch.Node) => Boolean) {
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

		search(node5, node1) should be(true)
		search(node6, node1) should be(false)
	}

	"DepthSearch" should "search for node in a graph" in {
		test(DepthSearch.search(_, _))
	}

	it should "search(immutable) for node in a graph" in {
		test(DepthSearch.searchImmutable)
	}

	it should "search(immutable, tailrec) for node in a graph" in {
		test(DepthSearch.searchImmutableTailRec(_, _))
	}

	case class TestNode(id: Int) extends DepthSearch.Node {
		var _neighbors = Set[TestNode]()

		override def neighbors: Iterable[DepthSearch.Node] = _neighbors
	}

}
