object Part1 extends App {
  val positions = InputParser.getPositionsFromFile("input.txt")
  println(Logic.findMinimumFuelUsageForLinearFuelUsage(positions))
}

object Part2 extends App {
  val positions = InputParser.getPositionsFromFile("input.txt")
  println(Logic.findMinimumFuelUsageForIncreasingFuelUsage(positions))
}


object Logic {
  def findMinimumFuelUsageForLinearFuelUsage(positions: Array[Int]): Int = {
    val median = findMedian(positions.sorted)
    positions.map(pos => math.abs(pos - median)).sum
  }

  private def findMedian(sortedPositions: Array[Int]): Int = {
    sortedPositions.length match {
      case it if it % 2 == 1 => sortedPositions(it / 2)
      case it => (sortedPositions(it / 2) + sortedPositions(it / 2 - 1)) / 2
    }
  }

  def findMinimumFuelUsageForIncreasingFuelUsage(positions: Array[Int]): Int = {
    var minUsage = Int.MaxValue
    val possiblePositions = 0 until positions.max

    for (i <- possiblePositions) {
      val usageAtPosition = positions.map(pos => {
        var spacesToMove = math.abs(pos - i)
        var fuelConsumption = 0
        var fuelConsumptionPerMovement = 0

        while (spacesToMove > 0) {
          fuelConsumptionPerMovement = fuelConsumptionPerMovement + 1
          fuelConsumption = fuelConsumption + fuelConsumptionPerMovement
          spacesToMove = spacesToMove - 1
        }

        fuelConsumption
      }).sum

      minUsage = math.min(minUsage, usageAtPosition)
    }
    minUsage
  }
}
