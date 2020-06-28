package week4.idealizednaturals

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("zero does not have predecessor")

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) this else throw new Error("Result would be negetive")

  override def toString: String = "0"
}
