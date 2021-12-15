import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

case class Fold(axis: Char, index: Int)

object Part1 extends App {
  val (coordinates, folds) = InputParser.parseInput("input.txt")
  val field = Logic.constructField(coordinates)

  val fieldAfterFolds = Logic.applyFolds(field, List(folds.head))
  val countOfMarked = fieldAfterFolds
    .flatten
    .count(x => x == '#')
  println(countOfMarked)
}

object Part2 extends App {
  val (coordinates, folds) = InputParser.parseInput("input.txt")
  val field = Logic.constructField(coordinates)

  val fieldAfterFolds = Logic.applyFolds(field, folds)
  fieldAfterFolds.foreach(r => {
    r.foreach(elem => print(elem match {
      case '#' => '#'
      case '.' => ' '
    }))
    println()
  })
}

object Logic {
  def constructField(coordinates: List[(Int, Int)]): Array[Array[Char]] = {
    var maxX = 0
    var maxY = 0

    coordinates.foreach(c => {
      maxX = math.max(maxX, c._1)
      maxY = math.max(maxY, c._2)
    })

    val field = Array.fill(maxY + 1) {
      Array.fill(maxX + 1) {
        '.'
      }
    }

    coordinates.foreach(c => field(c._2)(c._1) = '#')
    field
  }

  def applyFolds(field: Array[Array[Char]], folds: List[Fold]): Array[Array[Char]] = {
    var currentField = field

    for (fold <- folds) {
      fold match {
        case Fold('x', index) => currentField = applyFoldOnX(currentField, index)
        case Fold('y', index) => currentField = applyFoldOnY(currentField, index)
      }
    }

    currentField
  }

  def applyFoldOnX(field: Array[Array[Char]], index: Int): Array[Array[Char]] = {
    applyFoldOnY(field.transpose, index).transpose
  }

  def applyFoldOnY(field: Array[Array[Char]], index: Int): Array[Array[Char]] = {
    field
      .zipWithIndex
      .filter(rowWithIndex => rowWithIndex._2 > index)
      .foreach(rowWithIndex => {
        val copyOverRow = index - (rowWithIndex._2 - index)
        rowWithIndex
          ._1
          .zipWithIndex
          .filter(innerRowWithIndex => innerRowWithIndex._1 == '#')
          .foreach(innerRowWithIndex => field(copyOverRow)(innerRowWithIndex._2) = '#')
      })

    field
      .zipWithIndex
      .filter(rowWithIndex => rowWithIndex._2 < index)
      .map(rowWithIndex => rowWithIndex._1)
  }
}

object InputParser {
  def parseInput(fileName: String): (List[(Int, Int)], List[Fold]) = {
    Using.resource(Source.fromFile(fileName)) { source =>
      val coordinates: ListBuffer[(Int, Int)] = ListBuffer()
      val folds: ListBuffer[Fold] = ListBuffer()

      for (line <- source.getLines()) {
        line.strip() match {
          case _ if line.startsWith("fold") =>
            val parts = line.split(' ')(2).split('=')
            folds += Fold(parts(0)(0), parts(1).toInt)
          case "" =>
          case _ =>
            val parts = line.split(',').map(x => x.toInt)
            coordinates += ((parts(0), parts(1)))
        }
      }

      (coordinates.toList, folds.toList)
    }
  }
}