object MaxSubsequence {

  def find(xs: Seq[Int]) = {
    var resultSum = 0
    var fullSum = 0
    var minSum = 0
    xs.foreach { x =>
      fullSum += x
      resultSum = math.max(resultSum, fullSum - minSum)
      minSum = math.min(minSum, fullSum)
    }

    resultSum
  }

  def findKadane(xs: Seq[Int]) = {
    var maxSum = 0
    var sum = 0
    xs.foreach { x =>
      sum = math.max(0, sum + x)
      maxSum = math.max(sum, maxSum)
    }

    maxSum
  }
}
