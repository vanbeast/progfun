package recfun
import common._

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
    if (c == 0 || r == 1) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], opened: Int): Boolean = {
      if (chars.isEmpty) return opened == 0
      if (opened < 0) return false
      if (chars.head == '(') return balanceIter(chars.tail, opened+1)
      if (chars.head == ')') return balanceIter(chars.tail, opened-1)
      return balanceIter(chars.tail, opened)
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(sum: Int, coins: List[Int]): Int = {
      if (sum == money) return 1
      if (sum > money) return 0
      if (coins.isEmpty) return 0
      if (coins.tail.isEmpty) return count(sum + coins.head, coins)
      return count(sum+coins.head, coins) + count(sum, coins.tail)
    }
    if (money <= 0 || coins.isEmpty) return 0
    return count(0, coins)
  }
}
