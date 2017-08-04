import scala.collection.SortedSet

object HyperLogLog {
  def getApproximateSetCardinality(multiset: Iterable[_], accuracy: Int = 1) = {
    val processedHashes = multiset.view.map(x => math.abs(x.hashCode)).foldLeft(SortedSet[Int]()(Ordering[Int].reverse)) { (minHashes, hash) =>
      if (minHashes.size < accuracy)
        minHashes + hash
      else if (minHashes.head > hash && ! minHashes.contains(hash))
        minHashes - minHashes.head + hash
      else
        minHashes
    } map { hash =>
      hash / Integer.MAX_VALUE.asInstanceOf[Double]
    }
    1 / averagedIntervalFrom0ToMinHash(processedHashes)
  }

  private def averagedIntervalFrom0ToMinHash(hashes: SortedSet[Double]) =
    (hashes.toSeq.sliding(2).map(pair => pair(1) - pair(0)).sum + hashes.head) / hashes.size
}
