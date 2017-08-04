import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class MinHashSpec extends FlatSpec with Matchers {
  "MinHash" should "calculate and approximate Jaccard similarity coefficient of two collections" in {
    val (xs1, xs2) = testCollections
    val similarity = xs1.intersect(xs2).size / xs1.union(xs2).size.asInstanceOf[Double]

    val result = MinHash.getApproximateSimilarityRatio(xs1, xs2, 256)

    (math.abs(result - similarity) / similarity.asInstanceOf[Double]) should be < 0.02
  }

  private def testCollections = {
    val rand = new Random(1)
    var xs1 = Set[Int]()
    var xs2 = Set[Int]()
    for (i <- 1 to 65536)
      if (rand.nextBoolean()) {
        xs1 += rand.nextInt()
        xs2 += rand.nextInt()
      } else {
        val e = rand.nextInt()
        xs1 += e
        xs2 += e
      }
    (xs1, xs2)
  }
}
