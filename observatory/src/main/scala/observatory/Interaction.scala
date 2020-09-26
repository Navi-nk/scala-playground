package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.math.{Pi, atan, pow, sinh}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {
  val width: Int =256
  val height: Int =256
  val alpha: Int = 127
  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = pow(2, tile.zoom)
    val lon_deg = (tile.x / n) * 360.0 - 180.0
    val lat_rad = atan(sinh(Pi * (1 - 2 * tile.y / n)))
    val lat_deg = lat_rad * 180.0 / Pi
    Location(lat_deg, lon_deg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val offX = tile.x * width
    val offY = tile.y * height

    val allCords = for {
      i <- 0 until height
      j <- 0 until width
    } yield (i, j)

    val pixels = allCords.par
      .map({case (y, x) => Tile(x + offX, y + offY, tile.zoom + 8)})
      .map(tileLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, alpha))
      .toArray

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 until 4
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      yearData <- yearlyData
    } yield generateImage(yearData._1, Tile(x, y, zoom), yearData._2)
  }

}
