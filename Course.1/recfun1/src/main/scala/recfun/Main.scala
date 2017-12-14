package recfun

import scala.annotation.tailrec
import scala.collection.mutable

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
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case __ if __ >= r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */

  def balance(chars: List[Char]): Boolean = chars match {
    case x if x.isEmpty => true
    case x if x.head.equals(')') => false
    case _ => process(chars, new mutable.Stack[Char])
  }

  @tailrec
  def process(chars: List[Char], stack: mutable.Stack[Char]): Boolean = {
    if (chars.isEmpty) return true
    chars.head match {
      case '(' => process(chars.tail, stack.push(chars.head))
      case ')' => if (stack.nonEmpty && stack.top.equals('(')) {
        stack.pop
        process(chars.tail, stack)
      } else false
      case _ => process(chars.tail, stack)
    }
  }

  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = money match {
    case 0 => 1
    case x if x < 0 => 0
    case x if x >= 1 && coins.isEmpty => 0
    case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
