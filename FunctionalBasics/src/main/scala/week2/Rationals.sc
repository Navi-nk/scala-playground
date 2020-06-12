class Rational(x: Int, y: Int) {

  require(y != 0 , "Denominator cannot be zero")

  def this(x: Int) = this(x, 1)

  def numer: Int = x / g
  def denom: Int = y / g

  def abs(x:Int): Int = if(x<0) -x else x
  private val g : Int = GCD(abs(x),abs(y))

  private def GCD(x: Int, y: Int): Int = {
    if (y == 0) x else GCD(y, x % y)
  }
  def add(that: Rational) ={
    new Rational( numer * that. denom  + that.numer * denom, denom * that.denom)
  }

  def + (that: Rational) : Rational = {
    add(that)
  }

  def neg: Rational = {
    new Rational(-numer, denom)
  }

  def unary_- : Rational = this.neg

  def sub(that: Rational) : Rational = add(that.neg)

  def - (that: Rational): Rational = sub(that)

  def less(that: Rational) : Boolean = numer * that.denom < that.numer * denom

  def greater(that: Rational) : Boolean = !less(that)

  def < (that: Rational) : Boolean = less(that)

  def > (that: Rational) : Boolean = greater(that)

  def max(that : Rational) : Rational = if(less(that)) that else this

  override def toString : String = s"$numer/$denom"
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.neg            // -1/3
-x               // -1/3
x.add(y)         // 22/21
x + y            // 22/21
x.sub(y).sub(z)  //-79/42
x - y - z        // -79/42
y.add(y)         // 10/7
x.less(y)        // true
x < y            // true
x.greater(y)     // false
x > y            // false
x.max(y)         // 5/7
y.max(x)         // 5/7

val invalid = new Rational( 1, 0) // error

new Rational(100)
val a = new Rational(5, 16)
val b = new Rational(5, 12)

a.add(b)
