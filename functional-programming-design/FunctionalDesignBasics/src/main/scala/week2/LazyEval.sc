def expr = {
  val x = { print("x"); 1}
  lazy val y = {print("y"); 2}
  def z = {print("z"); 3}
  z + y + x + z + y + x
}

expr


def streamRange(l:Int, h:Int):Stream[Int] = {
  if(l>=h) Stream.empty
  else Stream.cons(l, streamRange(l+1, h))
}

streamRange(1,100).take(3).toList

def from(n:Int) : Stream[Int] = n #:: from(n+1)

val nats: Stream[Int] = from(1)
nats.take(10).toList
val m2s:Stream[Int] = nats.map(_ * 2)
m2s.take(100).toList

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter(_ % s.head != 0))
}

val primes:Stream[Int] = sieve(from(2))
primes.take(50).toList

def sqrtStream(x:Double):Stream[Double] = {
  def improve(guess:Double) = (guess + x / guess) / 2
  lazy val guesses:Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess:Double, x:Double) =
  math.abs(guess * guess - x)/ x < 0.00001

sqrtStream(9).filter(isGoodEnough(_, 9)).take(3).toList
