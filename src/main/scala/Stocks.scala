object Stocks {

  def maxProfit(prices: Array[Int]): Int = {

    @annotation.tailrec
    def findMaxProfit(prices: Array[Int], minPrice: Int, maxProfit: Int): Int = {
      if (prices.isEmpty) maxProfit
      else {
        val currentPrice = prices.head
        val newMinPrice = math.min(minPrice, currentPrice)
        val newMaxProfit = math.max(maxProfit, currentPrice - minPrice)
        findMaxProfit(prices.tail, newMinPrice, newMaxProfit)
      }
    }

    if (prices.isEmpty) 0
    else findMaxProfit(prices.tail, prices.head, 0)

  }

def main(args: Array[String]): Unit = {

  val prices = Array(7, 1, 5, 3, 6, 4)
  println("Maximum profit for prices: " + maxProfit(prices))

}

}

