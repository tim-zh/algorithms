import scala.collection.SortedSet

object AStar {
	def getPath[N <: Node[N]](start: N, finish: N): Option[Seq[N]] = {
		var resultPath = Seq[N]()
		var usedSet = Set[N]()
		var frontSet = SortedSet[N]()(Ordering.fromLessThan { (n1, n2) =>
			start.distanceTo(n1) + finish.distanceTo(n1) < start.distanceTo(n2) + finish.distanceTo(n2)
		})

		resultPath :+= start
		var currentBestNode = start

		while (frontSet.nonEmpty || (currentBestNode == start)) {
			frontSet = frontSet ++ currentBestNode.neighbors.filter(! usedSet.contains(_))
			usedSet = usedSet + currentBestNode
			currentBestNode = frontSet.head
			frontSet = frontSet.tail

			//it is like takeWhile plus one more
			val (a, b) = resultPath.span(n => ! n.neighbors.exists(currentBestNode == _))
			resultPath = a :+ b.head

			resultPath :+= currentBestNode

			if (currentBestNode == finish)
				return Some(resultPath)
		}
		None
	}

	trait Node[N <: Node[N]] {
		def distanceTo(n: N): Double

		def neighbors: Iterable[N]
	}

}
