def factorial(x: Double) : Double = {
  if (x == 0) 1 else x * factorial(x - 1)
}

def factorialTailRec(x: Double) : Double = {
  def factIter(x: Double, acc: Double) : Double = {
    if (x == 0) acc else factIter( x - 1 , acc * x)
  }
  factIter(x , 1.0)
}

factorial(6)
factorial(7)
factorial(8)

factorialTailRec(6)
factorialTailRec(7)
factorialTailRec(8)