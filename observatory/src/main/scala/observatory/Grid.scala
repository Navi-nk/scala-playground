package observatory

class Grid {

  private val gridArray: Array[Temperature] = new Array[Temperature](360 * 180)

  private def gridIndex(lat: Int, lon: Int): Int = {
    val x = lon + 180
    val y = lat + 89
    y * 360 + x
  }

  private def createCell(lat: Int, lon: Int, temperature: Temperature) {
    gridArray(gridIndex(lat, lon)) = temperature
  }

  def createGrid(temperatures: Iterable[(Location, Temperature)]) : Unit = {
    for {
      lat <- Range(90, -90, -1)
      lon <- -180 until 180
    } createCell(lat, lon, Visualization.predictTemperature(temperatures, Location(lat, lon)))
  }

  def accessCell(lat: Int, lon: Int) : Temperature = {
    gridArray(gridIndex(lat, lon))
  }

  def +=(that: Grid): Unit = {
    for( i <- 0 until this.gridArray.length) {
      this.gridArray(i) += that.gridArray(i)
    }
  }

  def divide(scalar: Int): Unit = {
    for( i <- 0 until this.gridArray.length) {
      this.gridArray(i) = this.gridArray(i) / scalar
    }
  }

  def -=(that: GridLocation => Temperature): Unit = {
    for {
      lat <- Range(90, -90, -1)
      lon <- -180 until 180
    } createCell(lat, lon, accessCell(lat, lon) - that(GridLocation(lat, lon)))
  }

}

object Grid {
  def apply(temperatures: Iterable[(Location, Temperature)]): Grid = {
    val grid = new Grid()
    grid.createGrid(temperatures)
    grid
  }
}
