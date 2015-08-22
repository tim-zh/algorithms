object MergeSort {

	def sort(xs: Seq[Int], mergeStrategy: (Seq[Int], Seq[Int]) => Seq[Int] = merge): Seq[Int] =
		if (xs.size < 2)
			xs
		else {
			val (a, b) = xs.splitAt(xs.length / 2)
			mergeStrategy(sort(a, mergeStrategy), sort(b, mergeStrategy))
		}

	def merge(xs1: Seq[Int], xs2: Seq[Int]): Seq[Int] =
		if (xs1.isEmpty)
			xs2
		else if (xs2.isEmpty)
			xs1
		else if (xs1.head <= xs2.head)
			xs1.head +: merge(xs1.tail, xs2)
		else
			xs2.head +: merge(xs1, xs2.tail)

	def mergeTailRec(xs1: Seq[Int], xs2: Seq[Int], result: Seq[Int] = Seq()): Seq[Int] =
		if (xs1.isEmpty)
			result ++ xs2
		else if (xs2.isEmpty)
			result ++ xs1
		else if (xs1.head <= xs2.head)
			mergeTailRec(xs1.tail, xs2, result :+ xs1.head)
		else
			mergeTailRec(xs1, xs2.tail, result :+ xs2.head)

	def mergeNonRec(xs1: Seq[Int], xs2: Seq[Int]): Seq[Int] = {
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
