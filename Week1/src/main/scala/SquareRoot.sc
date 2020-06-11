def abs(x:Double) = if (x < 0) -x else x

/** Here we are normalizing the diff by diving by x
  * since for very small values of x the epsilon of 0.001 is too large resulting in imprecise values
  * and for very large values of x the epsilon is very small and the function will not terminate
  * since the predicate will not be satisfied
  */
def isGoodEnough(guess: Double, x: Double) = {
  val diff = guess * guess - x
  val norm = abs(diff) / x
  println(s"diff: $diff")
  println(s"norm: $norm")
  norm < 0.001
}


def improve(guess: Double, x: Double) =(guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(2)
sqrt(1e-6)
sqrt(1e60)