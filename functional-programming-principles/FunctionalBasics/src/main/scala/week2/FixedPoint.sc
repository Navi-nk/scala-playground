// A value x is called fixed point if for some function f evaluates to x itself.
// i.e.,  f(x) = x

def abs(x:Double): Double = if(x<0) -x else x
val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double):Boolean =
  abs(x-y)/x / x  < tolerance
def fixedPoint(f : Double => Double)(firstGuess: Double): Double = {
  def iterate(guess:Double): Double = {
    val next: Double = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint( x => 1 + x/2)(1)

def averageDamping(f: Double => Double)(x : Double) = (x + f(x)) /2
def sqrt(x: Double): Double =
  fixedPoint(averageDamping(y => x / y))(1)

sqrt(2)
