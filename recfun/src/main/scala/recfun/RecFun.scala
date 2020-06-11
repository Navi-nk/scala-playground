package recfun

import scala.util.control.TailCalls.TailRec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1 else pascal(c - 1, r -1) + pascal(c , r -1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(value: List[Char], acc: Int) : Boolean = {
      if (value.isEmpty) {
        if(acc == 0) true else false
      }else{
        if(value.head == '(' ) {
          balanceIter(value.tail, acc + 1)
        }
        else if (value.head == ')') {
          if(acc > 0)
            balanceIter(value.tail, acc - 1)
          else
            false
        }
        else balanceIter(value.tail , acc)
      }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty || money < 0) 0
    else if(money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
