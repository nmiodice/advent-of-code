import scala.collection.mutable
import scala.reflect.ClassTag

class Board {
  var rows: Array[Array[Int]] = Array.empty
  var marked: Array[Array[Boolean]] = Array.empty
  var hasWon = false

  def appendRow(row: Array[Int]): Unit = {
    rows = rows :+ row
    marked = marked :+ Array.fill(row.length) {
      false
    }
  }

  def mark(number: Int): Unit = {
    for (i <- rows.indices) {
      for (j <- rows(i).indices) {
        if (rows(i)(j) == number) {
          marked(i)(j) = true
        }
      }
    }
  }

  def isWinner: Boolean = {
    // cache the win to speedup program
    if (hasWon) {
      return true
    }
    // check for wins in this direction...
    for (i <- rows.indices) {
      var allMarked = true
      for (j <- rows(i).indices) {
        allMarked = allMarked && marked(i)(j)
      }

      if (allMarked) {
        hasWon = true
        return true
      }
    }

    // check for wins in other direction...
    for (j <- rows(0).indices) {
      var allMarked = true
      for (i <- rows.indices) {
        allMarked = allMarked && marked(i)(j)
      }

      if (allMarked) {
        hasWon = true
        return true
      }
    }

    false
  }

  def size: Int = {
    rows.map(r => r.length).sum
  }

  def getFlattenedBoard: Array[Int] = {
    flatten(rows)
  }

  private def flatten[T: ClassTag](items: Array[Array[T]]): Array[T] = items.flatMap(x => x.toList)

  def getUnmarkedNumbers: Array[Int] = {
    val numbers = flatten(this.rows)
    val marked = flatten(this.marked)

    (numbers zip marked).filter(x => !x._2).map(x => x._1)
  }
}

class BoardIndex private() {

  private val index: mutable.HashMap[Int, mutable.HashSet[Board]] = mutable.HashMap()

  def this(boards: Set[Board]) {
    this()
    for (board <- boards) {
      for (number <- board.getFlattenedBoard) {
        if (!index.contains(number)) {
          index.put(number, mutable.HashSet())
        }

        index(number).add(board)
      }
    }
  }

  def boardsWithNumber(number: Int): mutable.HashSet[Board] = {
    index.getOrElse(number, mutable.HashSet())
  }
}
