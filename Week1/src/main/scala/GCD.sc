def GCD(x: Double, y: Double): Double = {
  if (y == 0) x else GCD(y, x % y)
}

GCD(2, 4)
GCD(7, 11)
GCD(1015,2340)