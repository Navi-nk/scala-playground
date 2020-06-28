package week4.idealizedboolean

object Main {

  def main(args: Array[String]): Unit = {
    val a:IBoolean = ITrue
    val b:IBoolean = IFalse

    println(a && a)       // true
    println(b && b)       // false
    println(a || a)       // true
    println(b || b)       // false
    println(a && b)       // false
    println(b && a)       // false
    println(a || b)       // true
    println(b || a)       // true
    println(a.unary_!)    // false
    println(b.unary_!)    // true
    println(a > b)        // true
    println(a < b)        // false
  }

}
