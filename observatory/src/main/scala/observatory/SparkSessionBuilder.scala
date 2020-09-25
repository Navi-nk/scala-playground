package observatory

import org.apache.spark.sql.SparkSession

object SparkSessionBuilder {

  def getSparkSession: SparkSession = SparkSession
    .builder()
    .appName("Scala-Capstone")
    .config("spark.master", "local")
    .getOrCreate()

}
