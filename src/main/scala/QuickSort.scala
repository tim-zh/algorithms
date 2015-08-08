object QuickSort {

  def sort(xs: Array[Int]): Array[Int] = {
    if (xs.length < 2)
      xs
    else {
      val pivot = xs.head
      val (first, second) = xs.partition(_ < pivot)
      if (first.isEmpty)
        xs.head +: sort(xs.tail)
      else if (second.isEmpty)
        sort(xs.tail) :+ xs.head
      else
        sort(first) ++ sort(second)
    }
  }

  def sortInPlace(xs: Array[Int]) = {
    innerSort(xs, 0, xs.length - 1)
  }

  private def innerSort(xs: Array[Int], firstIndex: Int, lastIndex: Int): Unit = {
    if (lastIndex == firstIndex)
      return
    var (first, last) = (firstIndex, lastIndex)
    val pivot = xs(last)
    while(first <= last) {
      if (xs(first) > pivot) {
        swap(xs, first, last)
        last -= 1
      } else
        first += 1
    }
    if (last == lastIndex && xs(firstIndex) > xs(lastIndex)) { //if pivot is max
      swap(xs, firstIndex, lastIndex)
      innerSort(xs, 0, lastIndex - 1)
    } else if (lastIndex - firstIndex > 1) {
      innerSort(xs, firstIndex, last)
      innerSort(xs, last + 1, lastIndex)
    }
  }

  private def swap(xs: Array[Int], i: Int, j: Int) = {
    val temp = xs(i)
    xs.update(i, xs(j))
    xs.update(j, temp)
  }
}
