package week1

import scala.util.Random


class MonteCarloPi(iter: Int) {
  def mcCount = {
    val randomX = new Random
    val randomY = new Random

    var hits = 0
    for(i <- 0 until iter) {
      val x = randomX.nextDouble
      val y = randomY.nextDouble

      if( x*x + y*y < 1) hits += 1
    }

    hits
  }

  // lamba = pi / 4 ; lambda = (count of points inside circle) / total points
  def piEstimate = 4.0 * mcCount / iter

  //Parallel version
  /* def piEstimatePar = {
     val ((pi1, pi2),(pi3,pi4)) = parallel(
       parallel(mcCount(iter/4), mcCount(iter/4)),
       parallel(mcCount(iter/4), mcCount(iter/4))
     )
     4.0 * (pi1 + pi2 + pi3 + pi4) / iter
   }*/

}

