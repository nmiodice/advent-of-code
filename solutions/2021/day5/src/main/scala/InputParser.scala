import scala.io.Source
import scala.util.Using

object InputParser {
  def parse(fileName: String): List[Line] = {
    var lines = List[Line]()
    Using(Source.fromFile(fileName)) { source => {
      for (line <- source.getLines()) {
        val points = parseInputLineToPoints(line)
        lines = lines :+ new Line(points._1, points._2)
      }
    }
    }

    lines
  }

  private def parseInputLineToPoints(line: String): ((Int, Int), (Int, Int)) = {
    val parts: Array[Array[Int]] = line
      .strip()
      .split(" -> ")
      .map(x => {
        x
          .strip()
          .split(",")
          .map(x => x.strip().toInt)
      })

    (
      (parts(0)(0), parts(0)(1)),
      (parts(1)(0), parts(1)(1)),
    )
  }
}

