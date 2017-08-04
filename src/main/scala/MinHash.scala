import scala.collection.SortedSet

object MinHash {
  def getApproximateSimilarityRatio(xs1: Iterable[_], xs2: Iterable[_], accuracy: Int = 1) = {
    val minHashes1 = getMinHashes(xs1, accuracy)
    val minHashes2 = getMinHashes(xs2, accuracy)
    (minHashes1 intersect minHashes2 size) / (minHashes1 union minHashes2 size).asInstanceOf[Double]
  }

  private def getMinHashes(xs: Iterable[_], accuracy: Int) =
    xs.view.map(_.hashCode).foldLeft(SortedSet[Int]()) { (minHashes, hash) =>
      if (minHashes.size < accuracy)
        minHashes + hash
      else if (minHashes.head < hash && ! minHashes.contains(hash))
        minHashes - minHashes.head + hash
      else
        minHashes
    }
}
