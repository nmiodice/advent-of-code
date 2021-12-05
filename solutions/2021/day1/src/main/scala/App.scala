import scala.io.Source
import scala.util.Using

object App {
  def TrackDepthFromInputFile(fileName: String, windowSize: Int): Int = {
    val depthTracker = new DepthStateTracker(windowSize)
    Using(Source.fromFile(fileName)) { source => {
      for (line <- source.getLines()) {
        val reading = line.toInt
        depthTracker.observe(reading)
      }
    }
    }
    depthTracker.increases()
  }
}
