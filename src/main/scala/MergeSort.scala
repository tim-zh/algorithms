object MergeSort {

  def sort(xs: Seq[Int]): Seq[Int] = {
    if (xs.size == 1)
      xs
    else {
      val (a, b) = xs.splitAt(xs.length / 2)
      mergeOrdered(sort(a), sort(b))
    }
  }

  private def mergeOrdered(xs1: Seq[Int], xs2: Seq[Int]): Seq[Int] = {
    var (a, b, result) = (xs1, xs2, Seq[Int]())
    while (a.nonEmpty && b.nonEmpty) {
      if (a.head <= b.head) {
        result :+= a.head
        a = a.tail
      } else {
        result :+= b.head
        b = b.tail
      }
    }
    if (a.nonEmpty)
      result ++= a
    if (b.nonEmpty)
      result ++= b
    result
  }
}
