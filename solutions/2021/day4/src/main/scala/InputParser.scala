import scala.io.Source
import scala.util.Using

object InputParser {
  def parse(fileName: String): (Array[Int], Set[Board]) = {
    var boards = Set[Board]()
    var draws: Array[Int] = Array.empty

    Using(Source.fromFile(fileName)) { source => {
      var currentBoard = new Board()

      for (line <- source.getLines()) {
        if (draws.length == 0) {
          draws = line.strip().split(",").map(x => x.toInt)
        } else {
          line.strip() match {
            case "" if currentBoard.size > 0 => {
              if (currentBoard.size > 0) {
                boards += currentBoard
                currentBoard = new Board()
              }
            }
            case it if it.nonEmpty => {
              currentBoard.appendRow(it.split("\\s+").map(x => x.toInt))
            }
            case _ =>
          }
        }
      }
    }
    }
    (draws, boards)
  }
}
