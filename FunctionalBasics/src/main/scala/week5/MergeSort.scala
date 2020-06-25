package week5

object MergeSort {

  def msort[T](xs: List[T])(implicit c : (T,T) => Boolean): List[T]  ={
    val n = xs.length / 2
    if(n == 0) xs
    else {
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }

  private def merge[T](xs : List[T], ys: List[T])(implicit c : (T,T) => Boolean): List[T] = {
    (xs , ys) match {
      case (l ,List()) => l
      case (List(), r) => r
      case (l:: ls , r :: rs) => if(c(l,r)) l :: merge(ls, ys) else r :: merge(xs , rs)
    }
  }

  def main(args: Array[String]): Unit = {
    implicit def intCompare(x: Int, y: Int) : Boolean = x < y
    implicit def stringCompare(x:String, y:String) : Boolean = x.compareTo(y) < 0
    println(merge(1 :: 3 :: List(5), 2 :: 4 :: List(6)))
    println(msort( 2314 :: 5616 :: 6656 :: 1 :: 42516 :: 67 :: 567 :: List(0)))
    println(msort(List("abc", "aab", "aba")))
  }
}
