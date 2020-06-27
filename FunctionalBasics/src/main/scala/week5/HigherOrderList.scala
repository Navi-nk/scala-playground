package week5

object HigherOrderList {

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y * y) :: squareList(ys)
    }

  def filter[T](xs: List[T])(p : T => Boolean): List[T] = {
    xs match {
      case Nil => xs
      case y :: ys => if(p(y)) y :: filter(ys)(p) else filter(ys)(p)
    }
  }

  def posInts(xs: List[Int]) : List[Int] = filter(xs)( x => x > 0)

  def squareListSimple(xs : List[Int]) : List[Int] = xs map (x => x * x)

  def main(args: Array[String]): Unit = {
    val l = 1 :: 2 :: -3 :: List(4)
    println(squareList(l))
    println(squareListSimple(l))
    println(posInts(l))
  }
}
