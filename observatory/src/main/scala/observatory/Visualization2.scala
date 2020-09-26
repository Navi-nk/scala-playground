package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  val alpha: Int = 256
  val width = 360
  val height = 180
  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val t1 = d00 * (1 - point.x) * (1 - point.y)
    val t2 = d10 * point.x * (1 - point.y)
    val t3 = d01 * (1 - point.x) * point.y
    val t4 = d11 * point.x * point.y
    t1 + t2 + t3 + t4
  }


  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val offX = tile.x * width
    val offY = tile.y * height

    val allCords = for {
      i <- 0 until height
      j <- 0 until width
    } yield (i, j)

    val pixels = allCords.par
      .map({case (y, x) => Tile(x + offX, y + offY, tile.zoom + 8)})
      .map(tileLocation)
      .map(interpolateGrid(grid, _))
      .map(interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, alpha))
      .toArray

    Image(width, height, pixels)
  }

  private def interpolateGrid(grid: GridLocation => Temperature,
                  loc: Location): Temperature = {
    val lat = loc.lat.toInt
    val lon = loc.lon.toInt
    val d00 = GridLocation(lat, lon)
    val d01 = GridLocation(lat + 1, lon)
    val d10 = GridLocation(lat, lon + 1)
    val d11 = GridLocation(lat + 1, lon + 1)
    val point = CellPoint(loc.lon - lon, loc.lat - lat)
    bilinearInterpolation(point, grid(d00), grid(d01), grid(d10), grid(d11))
  }

}
