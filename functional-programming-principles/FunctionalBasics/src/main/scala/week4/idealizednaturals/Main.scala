package week4.idealizednaturals

object Main {
  def main(args: Array[String]): Unit = {
    val a:Nat = Zero
    val b:Nat = new Succ(a)
    println(s"$a,$b")
    val c = createNat(10)
    val d = createNat(10000)

    println(s"$a, $b, $c, $d")

    println(c - b)
    println(b + d)
  }

  def createNat(value: Int, acc : Nat = Zero) : Nat = {
    if(value <= 0) acc
    else createNat(value - 1 , new Succ(acc))
  }
}
