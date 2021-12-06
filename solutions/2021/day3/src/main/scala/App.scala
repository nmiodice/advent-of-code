import scala.io.Source
import scala.util.Using

object App {
  def RunDiagnostics(inputFile: String): Int = {
    val diagnostics = new BinaryDiagnostics()

    Using(Source.fromFile(inputFile)) { source => {
      for (line <- source.getLines()) {
        diagnostics.ParseDiagnosticLine(line)
      }
    }
    }

    diagnostics.epsilon() * diagnostics.gamma()
  }
}
