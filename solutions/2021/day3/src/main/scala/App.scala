import scala.io.Source
import scala.util.Using

object App {
  def GetPowerUsage(inputFile: String): Int = {
    val diagnostics = new BinaryDiagnostics()
    parse(inputFile, diagnostics)
    diagnostics.powerUsage()
  }

  def GetLifeSupportRating(inputFile: String): Int = {
    val diagnostics = new BinaryDiagnostics()
    parse(inputFile, diagnostics)
    diagnostics.lifeSupportRating()
  }

  private def parse(inputFile: String, diagnostics: BinaryDiagnostics): Unit = {
    Using(Source.fromFile(inputFile)) { source => {
      for (line <- source.getLines()) {
        diagnostics.ParseDiagnosticLine(line)
      }
    }
    }
  }
}
