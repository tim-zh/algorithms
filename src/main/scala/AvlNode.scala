class AvlNode[T <: Comparable[T]] private (value: T, parent: AvlNode[T]) {
  private var left: AvlNode[T] = _
  private var right: AvlNode[T] = _
  private var balance: Int = 0
  private val isRoot = parent == null

  def newTree(v: T): AvlNode[T] = {
    if (v == null)
      return null
    val root = new AvlNode[T](v, null)
    root.add(v)
    root
  }

  def contains(v: T): Boolean = {
    if (v == null)
      false
    else if (v.compareTo(value) == 0)
      true
    else if (v.compareTo(value) > 0)
      right.contains(v)
    else
      left.contains(v)
  }

  /**
   * @return true if height has grown
   */
  def add(v: T): Boolean = {
    if (isRoot) {
      if (v == null)
        return false
      if (left == null)
        left = new AvlNode[T](v, this)
      else
        left.add(v)
    } else if (v.compareTo(value) < 0) {
      if (left == null) {
        left = new AvlNode[T](v, this)
        balance -= 1
        return balance != 0
      } else if (left.add(v)) {
        balance -= 1
        rightRotate()
      }
    } else if (v.compareTo(value) > 0) {
      if (right == null) {
        right = new AvlNode[T](v, this)
        balance += 1
        return balance != 0
      } else if (right.add(v)) {
        balance += 1
        leftRotate()
      }
    }
    false
  }

  def remove(v: T): Unit = {
    if (v == null)
      return
    if (v.compareTo(value) == 0) {
      ???
    } else if (v.compareTo(value) < 0) {
      if (left != null)
        left.remove(v)
    } else if (right != null)
      right.remove(v)
  }

  def toSeq: Seq[T] = {
    val leftSeq = if (left != null)
      left.toSeq
    else
      Seq()
    val rightSeq = if (left != null)
      right.toSeq
    else
      Seq()
    (leftSeq :+ value) ++ rightSeq
  }

  def height: Int = {
    var result = 1
    if (left != null)
      result += left.height
    if (right != null)
      result += right.height
    result
  }

  private def leftRotate() = {
    var newSubRoot: AvlNode[T] = null
    if (right.balance >= 0) {
      newSubRoot = right
      right = newSubRoot.left
      newSubRoot.left = this
    } else {
      newSubRoot = right.left
      right.left = newSubRoot.right
      newSubRoot.right = right
      right = newSubRoot.left
      newSubRoot.left = this
    }
    if (this eq parent.left)
      parent.left = newSubRoot
    else
      parent.right = newSubRoot
  }

  private def rightRotate() = {
    var newSubRoot: AvlNode[T] = null
    if (right.balance >= 0) {
      newSubRoot = right
      right = newSubRoot.left
      newSubRoot.left = this
    } else {
      newSubRoot = right.left
      right.left = newSubRoot.right
      newSubRoot.right = right
      right = newSubRoot.left
      newSubRoot.left = this
    }
    if (this eq parent.left)
      parent.left = newSubRoot
    else
      parent.right = newSubRoot
  }
}
