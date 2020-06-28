abstract class IntSet {
  def root : Int
  def include(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet): IntSet
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def root = elem
  override def include(x: Int) =
    if (x < elem) new NonEmpty(elem, left include x, right)
    else if (x > elem) new NonEmpty(elem ,left, right include x )
    else this

  override def contains(x: Int) : Boolean =
    if (x < elem) left contains x
    else if(x > elem) right contains x
    else true

  override def union(other: IntSet): IntSet = {
    println("this root:"+root+ " other root:" + other.root )
    ((left union right) union other) include elem
  }

  override def toString = "{" + left + elem + right + "}"
}
object Empty extends IntSet {
  def root = Int.MinValue
  override def include(x: Int) : IntSet = new NonEmpty(x, Empty, Empty)
  override def contains(x: Int): Boolean = false
  override def toString: String = "."
  override def union(other: IntSet): IntSet = other
}



val n1 = new NonEmpty(2, Empty, Empty)
val n2 = n1 include 4
val n3 = n2 include 1

val t1 = n3  include 0

val n4 = new NonEmpty(-2, Empty, Empty)
val n5 = n4 include -4
val n6 = n5 include 3

val t2 = n6  include 0

t1 union t2
t2 union t1

t1.root
t2.root
res0.root
res1.root

