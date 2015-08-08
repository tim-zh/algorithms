import scala.collection.mutable


class BloomFilter[-T](size: Int) {

  private val filter = new mutable.BitSet(size)

  def add(x: T): Unit = filter += x.hashCode % size

  def apply(x: T): Boolean = filter(x.hashCode % size)
}
