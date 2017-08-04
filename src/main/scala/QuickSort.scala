object QuickSort {
  def sort(xs: Array[Int]): Array[Int] =
    if (xs.length < 2)
      xs
    else {
      val (a, b) = xs.tail.partition(_ < xs.head)
      (sort(a) :+ xs.head) ++ sort(b)
    }

  def sortInPlace(xs: Array[Int]): Unit = {
    @inline
    def swap(i: Int, j: Int) = {
      val temp = xs(i)
      xs(i) = xs(j)
      xs(j) = temp
    }

    def _sort(start: Int, end: Int): Unit = {
      if (end - start > 1) {
        var split = start + 1
        for (i <- start + 1 until end) {
          if (xs(i) < xs(start)) {
            swap(i, split)
            split += 1
          }
        }
        swap(start, split - 1)
        _sort(start, split - 1)
        _sort(split, end)
      }
    }

    _sort(0, xs.length)
  }
}
