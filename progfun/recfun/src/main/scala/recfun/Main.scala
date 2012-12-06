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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (x, y) => if (x == y) {
      1
    } else {
      pascal(x, y - 1) + pascal(x - 1, y - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def doBalance(chars: List[Char], d: Int): Boolean = (chars, d) match {
      case (_, -1) => false
      case (List(), _) => (d == 0)
      case (char :: chars, _) => char match {
        case '(' => doBalance(chars, d + 1)
        case ')' => doBalance(chars, d - 1)
        case _   => doBalance(chars, d)
      }
    }
    doBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (_, List()) => 0
    case (_, x :: xs) =>
      if (money < 0) 0
      else countChange(money - x, x::xs) + countChange(money, xs)
  }
}
