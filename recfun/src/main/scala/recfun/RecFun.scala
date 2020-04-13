package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(s"${pascal(col, row)} ")
      }
      println()
    }

    println("balance strings")
    println(s"balanced ${balance("()".toList)}")
    println(s"balanced ${balance("(if (zero? x))".toList)}")
    println(s"balanced ${balance("(if (zero? x) max (/ 1 x))".toList)}")
    println(
      s"balanced ${balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)} "
    )
    println(s"not balanced ${balance(":-)".toList)}")
    println(s"not balanced ${balance("())(".toList)}")
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def pascalIter(c: Int, r: Int): Int = {
      if (c < 0 || c > r) 0
      else if (c == 0 || c == r) 1
      else pascalIter(c - 1, r - 1) + pascalIter(c, r - 1)
    }

    pascalIter(c, r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceInner(chars: List[Char], currentOpen: Int): Boolean = {
      if (chars.isEmpty) return currentOpen == 0
      else {
        if (chars.head == ')') {
          if (currentOpen > 0) balanceInner(chars.tail, currentOpen - 1)
          else false
        } else if (chars.head == '(')
          balanceInner(chars.tail, currentOpen + 1)
        else balanceInner(chars.tail, currentOpen)
      }
    }

    balanceInner(chars, 0)
  }

  // /**
  //  * Exercise 3
  //  */
  // def countChange(money: Int, coins: List[Int]): Int = ???
}
