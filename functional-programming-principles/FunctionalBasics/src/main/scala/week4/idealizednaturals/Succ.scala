package week4.idealizednaturals

//Peano numbers. using this we can write other numbers
class Succ(n : Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor

  override def toString: String = {
    accRec(this, 0)
  }

  @scala.annotation.tailrec
  private def accRec(current: Nat, counter: Int): String = {
    if(current.isZero) counter.toString
    else accRec(current.predecessor, counter + 1)
  }
}
