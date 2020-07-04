case class First(a:Int)
case class Second(b:Int)

val first = First(1)
implicit val firstImplicit: First = First(2)

val second = Second(3)
implicit val secondImplicit: Second = Second(4)

//all parameters are implicit
def testFunction(implicit f: First, s: Second) = f.a + s.b


//def testFunction1(f:First, implicit s: Second) = f.a + s.b - compile error

def testFunctionC(f:First)(implicit s: Second) =  f.a + s.b
testFunctionC(first)(second)

//to know the implicit value for type First
implicitly[First]


/*
If an implicit value of type Bar[Y] is required,
the compiler will look for implicit definitions in the following companion objects:

Bar, because it is a part of Bar[Y],
Y, because it is a part of Bar[Y],
Foo, because it is a parent type of Bar,
and X, because it is a parent type of Y.
However, the Baz companion object will not be visited.
 */
trait Foo[A]
trait Bar[A] extends Foo[A]
trait Baz[A] extends Bar[A]
trait X
trait Y extends X


trait A {
  implicit val x: Int = 0
}

trait B extends A {
  implicit val y: Int = 1
  def f(implicit n: Int) = n
}

//implicit y is taken instead of x since it is defined in closer scope of function f
// y is defined in a trait that extends A (which is where x is defined),
// y is more specific than x. Thus, there is no ambiguity and the compiler selects y.
val t= new B {
  println("output from function:" + f)
}

class Example[A](a: A) {
  val e = a
}

implicit val e = new Example(1)

//This is syntactic sugar on implicit of parameterized types
// def testFunctionS[T](a:T)(implicit b: Example[T])
//this implicit will flow down to inner
def testFunctionS[T: Example](a : T) = {
  def inner(implicit s : Example[T]) = (a,s.e)
  inner
}

testFunctionS(1)


trait LowPriorityImplicits {
  implicit val intOrdering: Ordering[Int] = Ordering.Int
}

implicit val intReverseOrdering: Ordering[Int] = Ordering.Int.reverse
println(List(1, 2, 3).min) //3 because of Ordering.Int.reverse


