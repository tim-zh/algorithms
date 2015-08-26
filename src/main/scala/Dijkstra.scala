import scala.collection.SortedSet

object Dijkstra {
	def calculatePathWeights(start: Node): Map[Node, Double] = {
		var weights = Map(start -> 0d)
		var notVisited = SortedSet[Node](start)(Ordering.fromLessThan { (n1, n2) => weights(n1) < weights(n2) })
		var visited = Set[Node]()

		while (notVisited.nonEmpty) {
			val currentNode = notVisited.head
			notVisited = notVisited.tail
			visited += currentNode

			currentNode.neighbors.filterNot(visited).foreach { neighbor =>
				val newWeight = weights(currentNode) + currentNode.weightOf(neighbor)
				if (weights.contains(neighbor)) {
					if (newWeight < weights(neighbor))
						weights = weights.updated(neighbor, newWeight)
				} else
					weights += neighbor -> newWeight

				notVisited += neighbor
			}
		}

		weights
	}

	trait Node {
		def weightOf(neighbor: Node): Double

		def neighbors: Iterable[Node]
	}

}
