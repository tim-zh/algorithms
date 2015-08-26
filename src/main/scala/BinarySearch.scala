object BinarySearch {
	def searchIndex[T <: Comparable[T]](x: T, xs: Array[T]): Int = {
		if (xs.length < 2) {
			if (xs.contains(x))
				0
			else
				-1
		} else {
			val c = x.compareTo(xs(xs.length / 2))
			if (c == 0)
				xs.length / 2
			else if (c < 0)
				searchIndex(x, xs.take(xs.length / 2 - 1))
			else {
				val i = searchIndex(x, xs.drop(xs.length / 2))
				if (i == -1)
					-1
				else
					xs.length / 2 + i
			}
		}
	}
}
