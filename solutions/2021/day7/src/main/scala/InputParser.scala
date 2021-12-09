import scala.io.Source
import scala.util.Using

object InputParser {
  def getPositionsFromFile(fileName: String): Array[Int] = {
    var positions = Array[Int]()
    Using.resource(Source.fromFile(fileName)) { source => {
      for (line <- source.getLines()) {
        positions = positions ++ line
          .strip()
          .split(",")
          .map(x => x.toInt)
      }
    }
    }

    positions
  }
}