package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def getAggregator(c: Int, r: Int) : Int = {
        if(r == 0 || r == 1 || c == 0) 1
        else if (c == 0 || c == r) 1
        else if (c == 1 || c == r - 1) r
        else getAggregator(c - 1, r - 1) + getAggregator(c, r - 1)
      }
      getAggregator(c,r)
    }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var openedBracketCount : Int = 0

      def countBrackets(chars: List[Char]): Boolean = {
        if (!chars.isEmpty) {
            if(chars.head == '(')
              openedBracketCount += 1
            else if(openedBracketCount > 0 && chars.head == ')')
              openedBracketCount -= 1
            else if (openedBracketCount == 0 && chars.head == ')')
              openedBracketCount = - Int.MaxValue
          countBrackets(chars.tail)
          }
        true
      }
      countBrackets(chars)
      openedBracketCount == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var totalAmount = 0
      def check(money: Int, coins: List[Int]) {
        if (!coins.isEmpty)
          if (money > coins.head) {
            check(money - coins.head, coins)
            check(money, coins.tail)
          }
          else if (money < coins.head) {
            check(money , coins.tail)
          }
          else if (money - coins.head == 0) {
            totalAmount += 1
          }
      }
      check(money, coins.sorted)
      totalAmount
    }
  }
