import scala.collection.mutable


object DepthSearch {

  def search(x: Node, root: Node, visited: mutable.Set[Node] = mutable.Set()): Boolean =
    if (x == root)
      true
    else {
      visited += root
      root.neighbors.filter(!visited(_)).exists(search(x, _, visited))
    }

  trait Node {

    def neighbors: Iterable[Node]
  }
}
