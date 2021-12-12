import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val paddedInput = InputParser.getEnergyLevelsWithPadding("input.txt")
  val (flashCount, _) = Logic.countFlashes(paddedInput, 100)
  println(flashCount)
}

object Part2 extends App {
  val paddedInput = InputParser.getEnergyLevelsWithPadding("input.txt")

  // note: the rounds here were determined by trial and error
  val (_, flashesPerRound) = Logic.countFlashes(paddedInput, 250)

  val totalInputCount = (paddedInput.length - 2) * (paddedInput.length - 2)
  val firstRoundWithAllFlashed = flashesPerRound
    .zipWithIndex
    .filter(x => x._1 == totalInputCount)
    .map(x => x._2 + 1) // convert from array index to round
    .head
  println(firstRoundWithAllFlashed)
}

object Logic {
  // a faster solution may be investigated if the array is quite large, or the number of rounds is quite large.
  // the solution here is brute force

  // Returns a tuple where the first element refers to the total number of flashes, and the second element
  // refers to the number of flashes in each round
  def countFlashes(paddedEnergyLevels: Array[Array[Int]], rounds: Int): (BigInt, Array[Int]) = {
    var flashCounts = BigInt(0)
    var flashesPerRound: Array[Int] = Array()

    for (_ <- 0 until rounds) {
      // every point that flashed this round
      var allFlashedPointsInRound: Set[(Int, Int)] = Set()

      // step 1: increase energy levels at all points
      for (i <- 1 until paddedEnergyLevels.length - 1) {
        for (j <- 1 until paddedEnergyLevels(i).length - 1) {
          paddedEnergyLevels(i)(j) = paddedEnergyLevels(i)(j) + 1
        }
      }

      // every point that flashed while the energy level settles, in a given "round" of settling
      var reconciliationFlashes = Set[(Int, Int)]()
      do {
        reconciliationFlashes = Set()

        // step 2: find points that yield a flash
        for (i <- 1 until paddedEnergyLevels.length - 1) {
          for (j <- 1 until paddedEnergyLevels(i).length - 1) {

            // look for flashes, but avoid a re-flash
            if (paddedEnergyLevels(i)(j) > 9 && !allFlashedPointsInRound.contains((i, j))) {
              reconciliationFlashes = reconciliationFlashes + ((i, j))
              allFlashedPointsInRound = allFlashedPointsInRound + ((i, j))
            }
          }
        }

        // step 3: apply the flashes
        reconciliationFlashes.foreach(point => {
          // update energy levels of surrounding points
          for (deltaI <- -1 to 1) {
            for (deltaJ <- -1 to 1) {
              if (!(deltaI == 0 && deltaJ == 0)) {
                paddedEnergyLevels(point._1 + deltaI)(point._2 + deltaJ) = paddedEnergyLevels(point._1 + deltaI)(point._2 + deltaJ) + 1
              }
            }
          }
        })

        // step 4: repeat until no flashes
      } while (reconciliationFlashes.nonEmpty)

      // reset flash points to energy 0 and add the flashed points to the count
      flashCounts = flashCounts + allFlashedPointsInRound.size
      flashesPerRound = flashesPerRound :+ allFlashedPointsInRound.size
      allFlashedPointsInRound.foreach(point => paddedEnergyLevels(point._1)(point._2) = 0)
    }

    (flashCounts, flashesPerRound)
  }
}

// copied from my day9 solution mostly
object InputParser {
  def getEnergyLevelsWithPadding(fileName: String, paddingValue: Int = 9): Array[Array[Int]] = {
    var heightMap: Array[Array[Int]] = Array()
    Using.resource(Source.fromFile(fileName)) { source =>
      for (line <- source.getLines()) {
        heightMap = heightMap :+ line.split("").map(x => x.toInt)
      }
    }
    padHeightMap(heightMap, paddingValue)
  }

  private def padHeightMap(heights: Array[Array[Int]], paddingValue: Int): Array[Array[Int]] = {
    val topRow = Array.fill(heights.length + 2) {
      paddingValue
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