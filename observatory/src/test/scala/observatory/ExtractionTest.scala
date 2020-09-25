package observatory

import java.nio.file.Paths

import org.junit.Assert._
import org.junit.Test

import scala.io.Source

class ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  @Test def `test`: Unit = {
  // Extraction.locateTemperatures(1975, "/stations.csv","/1975.csv")
    Visualization.computeDist(Location(+32.950,+065.567), Location(+70.933,-008.667))
  //  val temperatureContent = Source.fromInputStream(getClass.getResourceAsStream("/1975.csv"), "utf-8").getLines().toSeq
    assert(true)
  }


}
