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
      if (c == 0) 1
      else if (r == c) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkString(chars: List[Char], num: Int): Boolean = {
        if (num < 0) false
        else if (chars.isEmpty) num == 0
        else if (chars.head == ')') checkString(chars.tail, num-1)
        else if (chars.head == '(') checkString(chars.tail, num+1)
        else checkString(chars.tail, num)
      }
      checkString(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def checkChanges(currentAmount: Int, coinsRes: List[Int], numChanges: Int): Int = {
        if (money == 0) numChanges
        else if (coinsRes.isEmpty) numChanges
        else if (currentAmount < money) checkChanges(currentAmount + coinsRes.head, coinsRes, numChanges)
        else if (currentAmount > money) checkChanges(currentAmount - coinsRes.head, coinsRes.tail, numChanges)
        else checkChanges(0, coinsRes.tail, numChanges + 1)
      }
      checkChanges(0, coins, 0)
    }
  }
