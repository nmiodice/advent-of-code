import scala.io.Source
import scala.util.Using

object Part2 extends App {
  var depthTracker = new DepthStateTracker(3)
  Using(Source.fromFile("input.txt")) { source => {
    for (line <- source.getLines()) {
      val reading = line.toInt
      depthTracker.observe(reading)
    }
  }
  }
  println(depthTracker.increases())
}