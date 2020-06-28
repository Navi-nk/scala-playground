package week4

object InsertionSort {

  def isort(xs: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List() => x :: Nil
      case y :: ys => if (x > y) x :: xs else y :: insert(x , ys)
    }
    xs match {
      case List() => List()
      case y :: ys  => insert(y, isort(ys))
    }
  }



  def main(args: Array[String]): Unit = {
    println(isort( 7 :: 6 :: 5 :: 1 :: 2 :: 4 :: List(3)))

    println(isort( 7 :: 60 :: 15 :: 1000 :: 12 :: 4 :: List(331)))
  }

}
