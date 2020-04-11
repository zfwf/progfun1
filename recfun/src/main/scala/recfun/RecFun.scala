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

  // /**
  //  * Exercise 2
  //  */
  // def balance(chars: List[Char]): Boolean = ???

  // /**
  //  * Exercise 3
  //  */
  // def countChange(money: Int, coins: List[Int]): Int = ???
}
