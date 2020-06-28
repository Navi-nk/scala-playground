// For chess board of size n * n, find solutions where n queens can be placed on the board
// without causing collisions

def safeQueens(n: Int) : Set[List[Int]] = {
  def isSafe(queens: List[Int], col: Int): Boolean = {
    val row = queens.length
    val qPosition = (0 until row ) zip queens
    qPosition forall  {
      case (r , c) => col != c && math.abs(col - c) != row -r
    }
  }
  def placeQueens(k:Int): Set[List[Int]] =
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(queens, col)
      } yield queens :+ col
        //if(isSafe(queens, col))  queens :+ col --> this to get solutions where a queen is not required to be placed in a row i.e num of queens <= n
        //else queens :+ -1
  placeQueens(n)
}

def show(queens:List[Int]): String = {
  val lines = for (col <- queens)
    yield {
      val board = Vector.fill(queens.length)("* ")
      if(col >= 0 ) board.updated(col, "Q ").mkString
      else board.mkString
    }
  "\n" + (lines mkString "\n")
}

(safeQueens(3) map show) mkString("\n -- \n")
safeQueens(4)
(safeQueens(4) map show) mkString("\n -- \n")



