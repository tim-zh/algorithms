class AvlNode[T <: Comparable[T]] private (private var value: T, var parent: AvlNode[T]) {
  protected var left: AvlNode[T] = _
  protected var right: AvlNode[T] = _
  private var balance: Int = 0

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
   * @return true if height has increased
   */
  def add(v: T): Boolean = {
    if (v.compareTo(value) < 0) {
      if (left == null) {
        left = new AvlNode[T](v, this)
        balance -= 1
        return balance != 0
      } else if (left.add(v)) {
        balance -= 1
        return ! tryRotateRight()
      }
    } else if (v.compareTo(value) > 0) {
      if (right == null) {
        right = new AvlNode[T](v, this)
        balance += 1
        return balance != 0
      } else if (right.add(v)) {
        balance += 1
        return ! tryRotateLeft()
      }
    }
    false
  }

  /**
   * @return true if height has decreased
   */
  def remove(v: T): Boolean =
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
        val heightDecreased = remove(replacement.value)
        if (heightDecreased)
          if (balance > 0)
            balance -= 1
          else
            balance += 1
        value = replacement.value
        heightDecreased
      }
    } else if (v.compareTo(value) < 0) {
      if (left == null)
        false
      else if (left.remove(v)) {
        balance += 1
        tryRotateLeft()
      } else
        false
    } else if (right == null)
      false
    else if (right.remove(v)) {
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
   * @return true if rotation was performed
   */
  private def tryRotateLeft(): Boolean = {
    if (balance < 2)
      return false
    var newSubRoot: AvlNode[T] = null
    if (right.balance > 0) {
      newSubRoot = right
      balance = 0
      newSubRoot.balance = 0

      right = newSubRoot.left
      if (right != null)
        right.parent = this
      newSubRoot.left = this
      replaceBy(newSubRoot)
      parent = newSubRoot
      true
    } else if (right.balance == 0) {
      newSubRoot = right
      balance = 1
      newSubRoot.balance = -1

      right = newSubRoot.left
      if (right != null)
        right.parent = this
      newSubRoot.left = this
      replaceBy(newSubRoot)
      parent = newSubRoot
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

      right = newSubRoot.left
      if (right != null)
        right.parent = this
      newSubRoot.left = this
      replaceBy(newSubRoot)
      parent = newSubRoot
      true
    }
  }

  /**
   * @return true if rotation was performed
   */
  private def tryRotateRight(): Boolean = {
    if (balance > -2)
      return false
    var newSubRoot: AvlNode[T] = null
    if (left.balance < 0) {
      newSubRoot = left
      balance = 0
      newSubRoot.balance = 0

      left = newSubRoot.right
      if (left != null)
        left.parent = this
      newSubRoot.right = this
      replaceBy(newSubRoot)
      parent = newSubRoot
      true
    } else if (left.balance == 0) {
      newSubRoot = left
      balance = -1
      newSubRoot.balance = 1

      left = newSubRoot.right
      if (left != null)
        left.parent = this
      newSubRoot.right = this
      replaceBy(newSubRoot)
      parent = newSubRoot
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

      left = newSubRoot.right
      if (left != null)
        left.parent = this
      newSubRoot.right = this
      replaceBy(newSubRoot)
      parent = newSubRoot
      true
    }
  }

  private def replaceBy(replacement: AvlNode[T]) = {
    if (this eq parent.left)
      parent.left = replacement
    else
      parent.right = replacement
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

object AvlNode {
  private class RootNode[T <: Comparable[T]](value: T) extends AvlNode[T](value, null) {
    override def contains(v: T): Boolean = {
      if (left == null)
        false
      else
        left.contains(v)
    }

    override def add(v: T): Boolean = {
      if (v == null)
        return false
      if (left == null)
        left = new AvlNode[T](v, this)
      else
        left.add(v)
      false
    }

    override def remove(v: T): Boolean = {
      if (v == null)
        return false
      if (left == null)
        false
      else
        left.remove(v)
      false
    }

    override def toSeq: Seq[T] = if (left == null) Seq() else left.toSeq

    override def height: Int = if (left == null) 0 else left.height

    override def toString: String = "root"
  }

  def newTree[T <: Comparable[T]](v: T): AvlNode[T] = {
    if (v == null)
      return null
    val root = new RootNode[T](v)
    root.add(v)
    root
  }
}