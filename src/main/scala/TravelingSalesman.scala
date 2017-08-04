object TravelingSalesman {
  type Node = Array[Double]

  def findPath(nodes: Set[Node]): (Double, Seq[Node]) = {
    if (nodes.size <= 2)
      nodes.seq
    var bestResult = (Double.MaxValue, Seq[Node]())
    for (n <- nodes) {
      val result = findPath(n, nodes - n)
      if (result._1 < bestResult._1)
        bestResult = result
    }
    bestResult
  }

  private def findPath(node: Node, nodes: Set[Node]): (Double, Seq[Node]) =
    if (nodes.size == 1)
      (distanceBetween(node, nodes.head), Seq(node, nodes.head))
    else {
      var bestResult = (Double.MaxValue, Seq[Node]())
      for (n <- nodes) {
        val result = findPath(n, nodes - n)
        val length = distanceBetween(node, n) + result._1
        if (length < bestResult._1)
          bestResult = (length, node +: result._2)
      }
      bestResult
    }

  private def distanceBetween(node1: Node, node2: Node) =
    math.sqrt(node1.zip(node2).map(pair => (pair._1 - pair._2) * (pair._1 - pair._2)).sum)
}
