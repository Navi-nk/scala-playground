package observatory

import java.nio.file.Paths
import java.time.LocalDate

import scala.io.{BufferedSource, Source}

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  val session = SparkSessionBuilder.getSparkSession

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val temperatureContent =  Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8").getLines().toSeq
    val stationsContent =  Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8").getLines().toSeq

    val temperaturesRDD = session.sparkContext.parallelize(temperatureContent)
    val stationsRDD = session.sparkContext.parallelize(stationsContent)

    val temperatureData = temperaturesRDD
      .map(_.split(","))
      .filter(r => r.length == 5)
      .map(t => ((t(0), t(1)), (LocalDate.of(year, t(2).toInt, t(3).toInt), fahrenheitToCelsius(t(4).toDouble))))

    val stationsData = stationsRDD
      .map(_.split(","))
      .filter(r => r.length == 4)
      .map(t => ((t(0), t(1)), Location(t(2).toDouble, t(3).toDouble)))

    stationsData.join(temperatureData).mapValues{
      case (location, (date, temperature)) => (date, location, temperature)
    }.values.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records
      .groupBy(_._2)
      .mapValues(_.map(_._3))
      .mapValues( t => t.sum / t.size)
  }

  def fahrenheitToCelsius(fahrenheit: Temperature): Temperature =
    (fahrenheit - 32) * (5.0/9)

}
