import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val heights = InputParser.parseInput("input.txt")
  val lowPoints = Logic.findLowPoints(heights)

  val totalRiskLevel = lowPoints.map(p => heights(p._1)(p._2) + 1).sum
  println(totalRiskLevel)
}

object Part2 extends App {
  val heights = InputParser.parseInput("input.txt")
  val lowPoints = Logic.findLowPoints(heights)
  val basins = Logic.findBasins(heights, lowPoints)

  val answer = basins
    .map(coordinates => coordinates.size)
    .sorted
    .takeRight(3)
    .product

  println(answer)
}

object Logic {
  def findLowPoints(heights: Array[Array[Int]]): List[(Int, Int)] = {
    var points = List[(Int, Int)]()

    for (i <- 1 until heights.length - 1) {
      for (j <- 1 until heights(0).length - 1) {
        val currHeight = heights(i)(j)
        if (
          currHeight < heights(i - 1)(j)
            && currHeight < heights(i + 1)(j)
            && currHeight < heights(i)(j - 1)
            && currHeight < heights(i)(j + 1)) {
          points = points :+ ((i, j))
        }
      }
    }

    points
  }

  def findBasins(heights: Array[Array[Int]], lowPoints: List[(Int, Int)]): List[Set[(Int, Int)]] = {
    lowPoints.map(point => findBasinForPoint(heights, point))
  }

  private def findBasinForPoint(heights: Array[Array[Int]], point: (Int, Int)): Set[(Int, Int)] = {
    var basin: Set[(Int, Int)] = Set(point)
    var frontier: Set[(Int, Int)] = Set(point)

    def isPartOfBasin(heightAtPoint: Int, candidateX: Int, candidateY: Int): Boolean = {
      val isHigher = heights(candidateX)(candidateY) > heightAtPoint
      val isNotNine = heights(candidateX)(candidateY) != 9
      val isAlreadyExplored = basin.contains((candidateX, candidateY))

      isHigher && isNotNine && !isAlreadyExplored
    }

    while (frontier.nonEmpty) {
      var newFrontier: Set[(Int, Int)] = Set()
      frontier.foreach(point => {
        val (i, j) = point
        val heightAtPoint = heights(i)(j)

        for (p <- List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))) {
          if (isPartOfBasin(heights(i)(j), p._1, p._2)) {
            basin = basin + (p)
            newFrontier = newFrontier + (p)
          }
        }
      })

      frontier = newFrontier
    }

    basin
  }
}

object InputParser {
  def parseInput(fileName: String): Array[Array[Int]] = {
    var heightMap: Array[Array[Int]] = Array()
    Using.resource(Source.fromFile(fileName)) { source =>
      for (line <- source.getLines()) {
        heightMap = heightMap :+ line.split("").map(x => x.toInt)
      }
    }
    padHeightMap(heightMap)
  }

  private def padHeightMap(heights: Array[Array[Int]]): Array[Array[Int]] = {
    val topRow = Array.fill(heights.length + 2) {
      9
    }
    val bottomRow = topRow
    var paddedArray: Array[Array[Int]] = Array()


    paddedArray = paddedArray :+ topRow
    heights
      .map(row => 9 +: row :+ 9)
      .foreach(row => paddedArray = paddedArray :+ row)
    paddedArray = paddedArray :+ bottomRow

    paddedArray
  }
}
