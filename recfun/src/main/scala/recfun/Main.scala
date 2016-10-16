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
      def checkChanges(money: Int, coinsRes: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0) 0
        else if (coinsRes.isEmpty) 0
        else checkChanges(money - coinsRes.head, coinsRes) + checkChanges(money, coinsRes.tail)
      }
      checkChanges(money, coins.filter(_ <= money))
    }
  }
