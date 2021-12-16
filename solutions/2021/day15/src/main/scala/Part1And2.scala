import scala.io.Source
import scala.util.Using

object Part1 extends App {
  Timer.time("part 1", () => {
    val riskMatrix = InputParser.parseInput("input.txt")
    val optimalRisk = Logic.findOptimalPathRisk(riskMatrix)

    println(optimalRisk)
  })
}

object Part2 extends App {
  Timer.time("part 2", () => {
    val riskMatrix = InputParser.applyTiling(
      InputParser.parseInput("input.txt"),
      5
    )
    val optimalRisk = Logic.findOptimalPathRisk(riskMatrix)

    println(optimalRisk)
  })
}

object Timer {
  def time(opName: String, f: () => Unit): Unit = {
    val start = System.currentTimeMillis()
    f()
    val end = System.currentTimeMillis()

    println(f"<-- (timing data) $opName took ${end - start} millis -->")
  }
}

object Logic {
  def findOptimalPathRisk(riskMatrix: Array[Array[Int]]): Int = {
    val lenX = riskMatrix.length
    val lenY = riskMatrix(0).length

    // each element tracks two things:
    //  - a series of coordinates that indicate the path taken to that spot
    //  - the risk associated with that path
    val riskTracking = Array.fill(lenX) {
      Array.fill(lenY) { Int.MaxValue }
    }

    riskTracking(0)(0) = 0

    var searchFrontier = Set[(Int, Int)]((0, 0))
    while (searchFrontier.nonEmpty) {
      var newFrontier = Set[(Int, Int)]()
      for (originPoint <- searchFrontier) {
        val riskAtOrigin = riskTracking(originPoint._1)(originPoint._2)

        val possibleMoves = List(
          (originPoint._1 + 1, originPoint._2),
          (originPoint._1 - 1, originPoint._2),
          (originPoint._1, originPoint._2 + 1),
          (originPoint._1, originPoint._2 - 1))

        possibleMoves
          .filter(move => {
            move._1 >= 0 && move._1 < lenX && move._2 >= 0 && move._2 < lenY
          })
          .foreach(move => {
            val riskAtTarget = riskTracking(move._1)(move._2)
            val proposedRisk = riskAtOrigin + riskMatrix(move._1)(move._2)

            if (proposedRisk < riskAtTarget) {
              riskTracking(move._1)(move._2) = proposedRisk
              newFrontier = newFrontier + move
            }
          })

      }

      searchFrontier = newFrontier
    }

    riskTracking.last.last
  }
}

object InputParser {
  def parseInput(fileName: String): Array[Array[Int]] = {
    var riskMatrix: Array[Array[Int]] = Array()
    Using.resource(Source.fromFile(fileName)) { source =>
      for (line <- source.getLines()) {
        riskMatrix = riskMatrix :+ line.strip().toCharArray.map(x => x.toString.toInt)
      }
    }

    riskMatrix
  }

  def applyTiling(matrix: Array[Array[Int]], tileCount: Int): Array[Array[Int]] = {
    val lenX = matrix.length
    val lenY = matrix(0).length

    val resultMatrix = Array.fill(lenX * tileCount) {
      Array.fill(lenY * tileCount) {
        0
      }
    }

    for (x <- resultMatrix.indices) {
      val tileX: Int = x / lenX

      for (y <- resultMatrix(0).indices) {
        val tileY: Int = y / lenY
        val amountToAdd = tileX + tileY

        // results wrap from 9 -> 1, not 9 -> 0, so we cannot simply
        // rely on a modulo operation
        resultMatrix(x)(y) = matrix(x % lenX)(y % lenY) + amountToAdd
        while (resultMatrix(x)(y) > 9) {
          resultMatrix(x)(y) = resultMatrix(x)(y) - 9
        }
      }
    }

    resultMatrix
  }
}
