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

  def pack[T](xs:List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case y :: ys =>
      val (first, rest) = xs span (z => y == z)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T,Int)] =
    pack(xs) map (y => (y.head, y.length))

  @scala.annotation.tailrec
  def foldLeft[U, T](x: List[T], z: U)(op : (U, T) => U) : U = {
    x match {
      case Nil => z
      case y :: ys => foldLeft(ys, op(z, y))(op)
    }
  }

  def foldRight[U, T](x: List[T], z: U)(op : (T, U) => U) : U = {
    x match {
      case Nil => z
      case y :: ys => op(y, foldRight(ys, z)(op))
    }
  }

  def main(args: Array[String]): Unit = {
    val l = 1 :: 2 :: -3 :: List(4)
    println(squareList(l))
    println(squareListSimple(l))
    println(posInts(l))

    println(pack(1 :: 2 :: 2 :: 3 :: 3 :: 3 :: List(1))) //List(List(1), List(2, 2), List(3, 3, 3), List(1))
    println(encode(1 :: 2 :: 2 :: 3 :: 3 :: 3 :: List(1))) //List((1,1), (2,2), (3,3), (1,1))

    println(foldLeft(l,0)(_ + _))
    println(foldRight(l,0)(_ + _))

    println(foldRight(l, 100 :: -10 :: Nil) (_ :: _))
    //println(foldLeft(l, 100 :: Nil) (_ :: _)) compile error
  }
}
