import scala.collection.SortedSet

object HyperLogLog extends App{
	def getApproximateSetCardinality(multiset: Iterable[_], accuracy: Int = 1) = {
		val processedHashes = multiset.view.map(_.hashCode()).foldLeft(SortedSet[Int]()(Ordering[Int].reverse)) { (minHashes, hash) =>
			if (minHashes.size < accuracy)
				minHashes + hash
			else if (minHashes.head > hash && ! minHashes.contains(hash))
				minHashes - minHashes.head + hash
			else
				minHashes
		} map { hashCode =>
			(hashCode / Integer.MAX_VALUE.asInstanceOf[Double] + 1) / 2
		}
		processedHashes.size / processedHashes.sliding(2).map(pair => pair.tail.head - pair.head).sum
	}
}
