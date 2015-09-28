class AvlNode[T <: Comparable[T]](private var value: T, private var parent: AvlNode[T] = null) {
  require(value != null)

  protected var left: AvlNode[T] = _
  protected var right: AvlNode[T] = _
  private var balance: Int = 0

  def contains(v: T): Boolean =
    if (v == null)
      false
    else if (v.compareTo(value) == 0)
      true
    else if (v.compareTo(value) > 0)
      right.contains(v)
    else
      left.contains(v)

  def add(v: T) = {
    internalAdd(v)
    var root = this
    while (root.parent != null)
      root = root.parent
    root
  }

  /**
   * @return true if height has increased
   */
  private def internalAdd(v: T): Boolean = {
    if (v.compareTo(value) < 0) {
      if (left == null) {
        left = new AvlNode[T](v, this)
        balance -= 1
        return balance != 0
      } else if (left.internalAdd(v)) {
        balance -= 1
        return ! tryRotateRight()
      }
    } else if (v.compareTo(value) > 0) {
      if (right == null) {
        right = new AvlNode[T](v, this)
        balance += 1
        return balance != 0
      } else if (right.internalAdd(v)) {
        balance += 1
        return ! tryRotateLeft()
      }
    }
    false
  }

  def remove(v: T) = {
    internalRemove(v)
    var root = this
    while (root.parent != null)
      root = root.parent
    root
  }

  /**
   * @return true if height has decreased
   */
  private def internalRemove(v: T): Boolean =
    if (v.compareTo(value) == 0) {
      if (left == null && right == null) {
        replaceBy(null)
        true
      } else if (left == null) {
        replaceBy(right)
        true
      } else if (right == null) {
        replaceBy(left)
        true
      } else {
        val replacement = if (balance > 0) greaterNode else lesserNode
        val heightDecreased = internalRemove(replacement.value)
        if (heightDecreased)
          if (balance > 0)
            balance -= 1
          else
            balance += 1
        value = replacement.value
        heightDecreased
      }
    } else if (v.compareTo(value) < 0) {
      if (left != null && left.internalRemove(v)) {
        balance += 1
        tryRotateLeft()
      } else
        false
    } else if (right != null && right.internalRemove(v)) {
      balance -= 1
      tryRotateRight()
    } else
      false

  def toSeq: Seq[T] = {
    val leftSeq = if (left != null)
      left.toSeq
    else
      Seq()
    val rightSeq = if (right != null)
      right.toSeq
    else
      Seq()
    (leftSeq :+ value) ++ rightSeq
  }

  def height: Int =
    1 + (if (balance < 0) left.height else if (balance > 0) right.height else if (left == null) 0 else left.height)

  override def toString: String = "node(" + value + ", parent=" + (if (parent == null) "null" else parent.toString) + ")"

  /**
   * @return true if height has decreased
   */
  private def tryRotateLeft(): Boolean = {
    if (balance < 2)
      return false
    var newSubRoot: AvlNode[T] = null
    val result = if (right.balance > 0) {
      newSubRoot = right
      balance = 0
      newSubRoot.balance = 0
      true
    } else if (right.balance == 0) {
      newSubRoot = right
      balance = 1
      newSubRoot.balance = -1
      false
    } else {
      newSubRoot = right.left
      right.left = newSubRoot.right
      if (left.right != null)
        right.left.parent = right
      newSubRoot.right = right
      newSubRoot.right.parent = newSubRoot
      right.balance = 0
      balance = 0
      true
    }
    right = newSubRoot.left
    if (right != null)
      right.parent = this
    newSubRoot.left = this
    replaceBy(newSubRoot)
    parent = newSubRoot

    result
  }

  /**
   * @return true if height has decreased
   */
  private def tryRotateRight(): Boolean = {
    if (balance > -2)
      return false
    var newSubRoot: AvlNode[T] = null
    val result = if (left.balance < 0) {
      newSubRoot = left
      balance = 0
      newSubRoot.balance = 0
      true
    } else if (left.balance == 0) {
      newSubRoot = left
      balance = -1
      newSubRoot.balance = 1
      false
    } else {
      newSubRoot = left.right
      left.right = newSubRoot.left
      if (right.left != null)
        left.right.parent = left
      newSubRoot.left = left
      newSubRoot.left.parent = newSubRoot
      left.balance = 0
      balance = 0
      true
    }
    left = newSubRoot.right
    if (left != null)
      left.parent = this
    newSubRoot.right = this
    replaceBy(newSubRoot)
    parent = newSubRoot

    result
  }

  private def replaceBy(replacement: AvlNode[T]) = {
    if (parent != null) {
      if (this eq parent.left)
        parent.left = replacement
      else
        parent.right = replacement
    }
    if (replacement != null)
      replacement.parent = parent
  }

  private def lesserNode = {
    var result = left
    while (result.right != null)
      result = result.right
    result
  }

  private def greaterNode = {
    var result = right
    while (result.left != null)
      result = result.left
    result
  }
}
