package week4.idealizedboolean

object ITrue extends IBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = t
  override def toString: String = "true"
}
