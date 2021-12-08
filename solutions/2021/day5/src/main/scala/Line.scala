import scala.math.{max, min}

class Line(private val p1: (Int, Int), private val p2: (Int, Int)) {

  def covers: Set[(Int, Int)] = {
    var coverage = Set[(Int, Int)]()
    if (isHorizontal) {
      for (y <- min(y1, y2) to max(y1, y2)) {
        coverage = coverage + ((x1, y))
      }
    } else if (isVertical) {
      for (x <- min(x1, x2) to max(x1, x2)) {
        coverage = coverage + ((x, y1))
      }
    } else {
      // assumed to be 45 degrees, per the problem prompt!
      val leftPoint = if (x1 < x2) p1 else p2
      val rightPoint = if (x1 < x2) p2 else p1

      val yStep = if (leftPoint._2 > rightPoint._2) -1 else 1

      var p = leftPoint
      coverage = coverage + p
      while (p != rightPoint) {
        p = (p._1 + 1, p._2 + yStep)
        coverage = coverage + p
      }
    }

    coverage
  }

  private def y1: Int = p1._2

  private def y2: Int = p2._2

  def isHorizontal: Boolean = x1 == x2

  private def x1: Int = p1._1

  private def x2: Int = p2._1

  def isVertical: Boolean = y1 == y2
}
