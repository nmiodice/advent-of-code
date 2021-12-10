import scala.io.Source
import scala.util.Using

object InputParser {
  def parseInputFile(fileName: String): List[DisplayGroup] = {
    var results: List[DisplayGroup] = List()

    Using.resource(Source.fromFile(fileName)) { source =>
      for (line <- source.getLines()) {
        val parts = line.strip().split("""\|""").map(x => x.strip())

        val tenSignals = parts(0)
          .split(" ")
          .map(x => x.strip())
          .map(x => DisplayPattern.fromString(x))

        val fourOutputValues = parts(1)
          .split(" ")
          .map(x => x.strip())
          .map(x => DisplayPattern.fromString(x))

        results = results.concat(List(new DisplayGroup(tenSignals, fourOutputValues)))
      }
    }

    results
  }
}
