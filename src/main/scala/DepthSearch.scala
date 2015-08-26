import scala.collection.immutable.Stack
import scala.collection.mutable

object DepthSearch {
	def search(x: Node, root: Node, visited: mutable.Set[Node] = mutable.Set()): Boolean =
		if (x == root)
			true
		else {
			visited += root
			root.neighbors.filterNot(visited).exists(search(x, _, visited))
		}


	def searchImmutable(x: Node, root: Node): Boolean = searchVertical(x, root, Set())._1

	private def searchVertical(x: Node, current: Node, visited: Set[Node]): (Boolean, Set[Node]) =
		if (x == current)
			(true, visited)
		else
			searchHorizontal(x, current.neighbors.filterNot(visited), visited + current)

	private def searchHorizontal(x: Node, neighbors: Iterable[Node], visited: Set[Node]): (Boolean, Set[Node]) = {
		if (neighbors.isEmpty)
			(false, visited)
		else if (visited contains neighbors.head)
			searchHorizontal(x, neighbors.tail, visited)
		else {
			val result = searchVertical(x, neighbors.head, visited)
			if (result._1)
				result
			else
				searchHorizontal(x, neighbors.tail, visited ++ result._2)
		}
	}


	def searchImmutableTailRec(x: Node, current: Node, parents: Stack[Node] = Stack(), visited: Set[Node] = Set()): Boolean =
		if (x == current)
			true
		else {
			val neighbors = current.neighbors.filterNot(visited)
			if (neighbors.isEmpty) {
				if (parents.isEmpty)
					false
				else {
					val parent = parents.head
					searchImmutableTailRec(x, parent, parents.pop, visited + current)
				}
			} else
				searchImmutableTailRec(x, neighbors.head, parents :+ current, visited + current)
		}


	trait Node {
		def neighbors: Iterable[Node]
	}

}
