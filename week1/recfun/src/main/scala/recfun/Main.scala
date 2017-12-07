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
      if (c < 0 || c > r )
        throw new java.util.InputMismatchException("invalid input")

      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balancePartly(chars: List[Char], nbrOpened: Int) :Boolean = {
        if(chars.isEmpty) true

        else {
          val newNbrOpened = {
            if(chars.head == ')') nbrOpened - 1
            else if(chars.head == '(') nbrOpened + 1
            else nbrOpened
          }

          if (newNbrOpened < 0) false
          else balancePartly(chars.tail, newNbrOpened)
        }
      }

      balancePartly(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if (coins.isEmpty || money < 0)
        0
      else if(money == 0) {
        1
      } else {
       countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
}
