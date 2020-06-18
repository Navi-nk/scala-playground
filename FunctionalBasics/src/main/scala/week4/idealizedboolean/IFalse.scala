package week4.idealizedboolean

object IFalse extends IBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = e

  override def toString: String = "false"
}
