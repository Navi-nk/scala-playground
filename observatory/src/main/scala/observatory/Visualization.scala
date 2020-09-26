package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{abs, sin, cos, sqrt, pow, toRadians, acos, asin}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {
  val earthRadius = 6371
  val p_param = 6
  val width = 360
  val height = 180
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distanceTpl = temperatures.map(entry => (earthRadius * computeDist(location, entry._1), entry._2))

    val min = distanceTpl.toSeq.sortWith((a,b) => a._1 < b._1).head
    if(min._1 < 1) {
      min._2
    }else{
      val idw = distanceTpl.map(entry => (1 / pow(entry._1, p_param), entry._2))
      val denom = idw.map(_._1).sum
      idw.map(entry => entry._1 * entry._2).sum  / denom
    }
  }

  def isAntipodes(a: Location, b: Location): Boolean = {
    (a.lat == -b.lat) && (abs(a.lon - b.lon) == 180)
  }

  def computeDist(a: Location, b: Location): Double = {
    if(a == b){
      0
    }else if(isAntipodes(a, b)){
       math.Pi
    } else {
      val alon = toRadians(a.lon)
      val blon = toRadians(b.lon)
      val alat = toRadians(a.lat)
      val blat = toRadians(b.lat)
      val deltaLongitude = abs(alon - blon)
      val deltaSigma =  acos((sin(alat) * sin(blat)) + (cos(alat) * cos(blat) * cos(deltaLongitude)))
      deltaSigma
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

      points.find(_._1 == value) match {
      case Some((_, col)) => col
      case _ =>
        val (smaller, bigger) = points.partition(_._1 < value)
        if (smaller.isEmpty) {
          bigger.minBy(_._1)._2
        }else {
          val a = smaller.maxBy(_._1)
          if (bigger.isEmpty) {
            a._2
          }else {
            val b = bigger.minBy(_._1)
            val wa = 1 / abs(a._1 - value)
            val wb = 1 / abs(b._1 - value)
            val ca = a._2
            val cb = b._2

            def interpolate(x: Int, y: Int): Int =
              ((wa * x + wb * y) / (wa + wb)).round.toInt

            Color(interpolate(ca.red, cb.red), interpolate(ca.green, cb.green), interpolate(ca.blue, cb.blue))
          }
        }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    def transformCoord(coord: (Int, Int)): Location = {
      val lon = (coord._2 - width/2)
      val lat = -(coord._1 - height/2)
      Location(lat, lon)
    }

    val coords = for {
      i <- 0 until height
      j <- 0 until width
    } yield (i, j)
    val pixels = coords.par
      .map(transformCoord)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, 255))
      .toArray
    Image(width, height, pixels)
  }

}

