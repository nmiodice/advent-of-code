import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val pair = Calculator.add(InputParser.parseInput("input.txt"): _*)
  println(pair.magnitude)
}

object Part2 extends App {
  val numbers = InputParser.parseInput("input.txt")

  val largestMagnitude = numbers
    .flatMap(n1 => numbers.map(n2 => (n1, n2)))
    .filter({ case (a: Component, b: Component) => a != b })
    .map({ case (a: Pair, b: Pair) => (a.copy, b.copy) })
    .map({ case (a: Pair, b: Pair) => Calculator.add(a, b).magnitude })
    .max

  println(largestMagnitude)
}

object Calculator {
  def add(pairs: Pair*): Component = {
    pairs.reduceLeft((x: Pair, y: Pair) => {
      val p = new Pair(x, y)
      p.reduce()
      p
    })
  }
}


object InputParser {
  def parseInput(fileName: String): List[Pair] = {
    Using.resource(Source.fromFile(fileName)) { source =>
      source
        .getLines()
        .map(l => parseInputLine(l))
        .toList
    }
  }

  def parseInputLine(line: String): Pair = {
    new Pair(
      parseLeft(line),
      parseRight(line)
    )
  }

  private def parseLeft(line: String): Component = {
    if (line.charAt(1).isDigit) {
      var i = 1
      while (line.charAt(i).isDigit) {
        i = i + 1
      }

      new NumericComponent(line.substring(1, i).toInt)
    } else {
      val leftParts = parseComponentUsingStackMarkers(line, '[', ']')
      parseInputLine(leftParts)
    }
  }

  private def parseRight(line: String): Component = {
    if (line.charAt(line.length - 2).isDigit) {
      var i = line.length - 2
      while (line.charAt(i).isDigit) {
        i = i - 1
      }

      new NumericComponent(line.substring(i + 1, line.length - 1).toInt)
    } else {
      val reversed = line.reverse
      val rightPartsReversed = parseComponentUsingStackMarkers(reversed, ']', '[')
      val rightParts = rightPartsReversed.reverse
      parseInputLine(rightParts)
    }
  }

  private def parseComponentUsingStackMarkers(line: String, stackStart: Char, stackEnd: Char): String = {
    var stackCount = 0
    val componentParts = line.substring(1).takeWhile(c => {
      c match {
        case _ if c == stackStart => stackCount = stackCount + 1
        case _ if c == stackEnd => stackCount = stackCount - 1
        case _ =>
      }

      stackCount != 0
    })

    // the logic above will miss the final end marker so we need to remember to add it back!
    componentParts + stackEnd
  }
}