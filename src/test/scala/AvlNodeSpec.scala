import org.scalatest.{Matchers, FlatSpec}

class AvlNodeSpec extends FlatSpec with Matchers {
  "AvlNode" should "make a balanced search tree" in {
    var tree = new AvlNode[Integer](20)
    for (i <- 19 to 1 by -1)
      tree = tree.add(i)

    val seq = tree.toSeq

    for (i <- 1 to 20)
      seq(i - 1) should be(i)
    seq.size should be(20)

    tree.height should equal(5)
  }

  it should "rebalance on removal" in {
    var tree = new AvlNode[Integer](1)
    for (i <- 2 to 20)
      tree = tree.add(i)

    for (i <- 1 to 12)
      tree = tree.remove(i)

    val seq = tree.toSeq

    for (i <- 13 to 20)
      seq(i - 13) should be(i)
    seq.size should be(8)

    tree.height should equal(3)
  }
}
