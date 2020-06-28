def isPrime(n:Int) : Boolean=
  (2 until n) forall ( z => n % z != 0)

//Pairs of all integers less than N such that sum of pairs is prime and first element is greater then second element
def search(n:Int) : List[(Int,Int)]=
  (1 until n) flatMap  ( i =>
    (1 until i) map (j => (i, j))) filter { case (x,y) => isPrime(x + y) } toList

search(7) //List((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5))

case class Person(name:String, age:Int)

def canDrink(persons : List[Person]): List[String] =
  for ( p <- persons if p.age >= 18) yield p.name

def canDrinkInUs(persons: List[Person]): List[String] =
  persons filter (_.age >= 21) map (_.name)

def readableSearch(n:Int): List[(Int,Int)] =
  (for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i,j)).toList

readableSearch(7)

def scalarProduct(xs: Vector[Int], ys: Vector[Int]): Int =
  (for ((x,y) <- xs zip ys ) yield x * y ).sum

scalarProduct(Vector(1,2,3), Vector(2,3,4))




