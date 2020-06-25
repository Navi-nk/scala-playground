package week5

object MergeSort {

  def msort(xs: List[Int]): List[Int]  ={
    val n = xs.length / 2
    if(n == 0) xs
    else {
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }

  private def merge(xs : List[Int], ys: List[Int]): List[Int] = {
    (xs , ys) match {
      case (l ,List()) => l
      case (List(), r) => r
      case (l:: ls , r :: rs) => if(l < r) l :: merge(ls, ys) else r :: merge(xs , rs)
    }
  }

  def main(args: Array[String]): Unit = {
    println(merge(1 :: 3 :: List(5), 2 :: 4 :: List(6)))
    println(msort( 2314 :: 5616 :: 6656 :: 1 :: 42516 :: 67 :: 567 :: List(0)))
  }
}
