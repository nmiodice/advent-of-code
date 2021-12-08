import scala.io.Source
import scala.util.Using

object InputParser {
  def getStateFromFile(fileName: String): Map[Int, BigInt] = {
    var ageCounts = Map[Int, BigInt]()
    Using.resource(Source.fromFile(fileName)) { source => {
      for (line <- source.getLines()) {
        line
          .strip()
          .split(",")
          .map(x => x.toInt)
          .foreach({ age => {
            val oldCount = ageCounts.getOrElse(age, BigInt(0))
            ageCounts = ageCounts + (age -> (oldCount + 1))
          }
          })
      }
    }
    }

    ageCounts
  }
}
