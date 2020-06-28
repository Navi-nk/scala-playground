package week6

object PolynomialUsingMaps {

  class Polynomial(val terms: Map[Int, Double]) {
    def this(t: (Int,Double)*) = this(t.toMap)
    def + (other: Polynomial) = new Polynomial(terms ++ (other.terms map adjust))

    def add(other:Polynomial) = new Polynomial((other.terms foldLeft terms)(addTerms))
    def addTerms(terms:Map[Int,Double], term: (Int,Double)): Map[Int, Double] = {
      val (e,c) = term
      terms get e match {
        case Some(coef) => terms + (e -> (c + coef))
        case None => terms + (e -> c)
      }
    }
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (e,c) = term
      terms get e match {
        case Some(coef) => e -> (c + coef)
        case None => e -> c
      }
    }

    override def toString: String =
      (for((exp,coeff) <- terms.toList.sorted.reverse) yield coeff+"x^" + exp ) mkString(" + ")
  }

  val p1 = new Polynomial(1 -> 2.0, 2-> 4.0, 3 -> 5.0)
  val p2 = new Polynomial(Map(1 -> 3.0, 2-> 7.1, 4 -> 2.6))

  println(p1 + p2)
  println(p1 add p2)


}
