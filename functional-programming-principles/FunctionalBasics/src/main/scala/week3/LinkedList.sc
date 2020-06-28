
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](x:U) : List[U] = new Cons(x, this)
}

class Cons[T](val head: T, val tail : List[T]) extends List[T] {
  override def isEmpty = false

  override def toString: String = s"$head->${tail.toString}"
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new java.util.NoSuchElementException

  override def tail: Nothing = throw new java.util.NoSuchElementException

  override def toString: String = "nil"
}

def singleton[T](elem: T) = new Cons[T](elem, Nil)

//type erasure and type inferred
singleton(1)
singleton(1.0)

def nth[T](n:Int, list: List[T]) : T = {
    if(list.isEmpty) throw new IndexOutOfBoundsException
    else if(n == 0) list.head
    else nth(n-1, list.tail)
}

def createList[T](values:Seq[T]) : List[T] = {
    if(values.size == 1) new Cons(values.head, Nil)
    else new Cons(values.head, createList(values.tail))
}

val testList: Cons[Int] = singleton(4)
nth(0, testList)

val testBiggerList = createList(Seq(1,2,3,4,5,6))
nth(6,testBiggerList)

