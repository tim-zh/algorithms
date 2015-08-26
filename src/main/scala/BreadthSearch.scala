import scala.collection.mutable

object BreadthSearch {
	def search(x: Node, root: Node) = {
		var result = false

		var visited = Set[Node]()
		val front = mutable.LinkedHashSet(root)

		while (front.nonEmpty) {
			val current = front.head
			front -= current
			visited += current
			if (current.neighbors.exists(x == _))
				result = true
			else
				front ++= current.neighbors.filterNot(visited)
		}
		result
	}

	trait Node {
		def neighbors: Iterable[Node]
	}

}
