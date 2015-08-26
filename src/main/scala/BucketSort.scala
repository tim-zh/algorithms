import scala.collection.mutable

object BucketSort {
	def sort(xs: Seq[Int], numberOfBuckets: Int = 4, baseAlgorithm: Option[Seq[Int] => Seq[Int]] = None): Seq[Int] = {
		if (xs.size <= numberOfBuckets * 2) {
			if (baseAlgorithm.isDefined)
				baseAlgorithm.get(xs)
			else
				xs.sorted
		} else {
			val min = xs.min
			val max = xs.max
			val buckets = for (i <- 1 to numberOfBuckets)
				yield mutable.ArrayBuilder.make[Int]()
			val bucketLength = (max - min) / numberOfBuckets
			xs foreach { x =>
				val i = (x - min) / bucketLength
				buckets(i) += x
			}
			buckets.
					map(b => sort(b.result(), numberOfBuckets, baseAlgorithm)).
					reduce((s1, s2) => s1 ++ s2)
		}
	}
}
