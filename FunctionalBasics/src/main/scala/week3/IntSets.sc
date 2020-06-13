abstract class IntSet {
  def include(x: Int) : IntSet
  def contains(x: Int) : Boolean
}

class Empty extends IntSet {
  override def include(x: Int) : IntSet = new NonEmpty(x, new Empty, new Empty)
  override def contains(x: Int): Boolean = false
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def include(x: Int) =
    if (x < elem) new NonEmpty(elem, left include x, right)
    else if (x > elem) new NonEmpty(elem ,left, right include x )
    else this

  override def contains(x: Int) : Boolean =
    if (x < elem) left contains x
    else if(x > elem) right contains x
    else true
}