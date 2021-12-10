import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val lines = InputParser.parseInput("input.txt")
  val score = Scorer.scoreLinesWithSyntaxErrors(lines)
  println(score)
}

object Part2 extends App {
  val lines = InputParser.parseInput("input.txt")
  val score = Scorer.scoreLinesWithAutocompleteNeeded(lines)
  println(score)
}

object Scorer {
  def scoreLinesWithSyntaxErrors(lines: Array[NavigationLine]): Int = {
    lines.map(line => {
      line.firstSyntaxError match {
        case None => 0
        case Some(illegalCharacter) => {
          illegalCharacter match {
            case ')' => 3
            case ']' => 57
            case '}' => 1197
            case '>' => 25137
          }
        }
      }
    }).sum
  }

  def scoreLinesWithAutocompleteNeeded(lines: Array[NavigationLine]): BigInt = {
    val scores = lines
      .map(l => l.autoComplete)
      .filter(x => x.isDefined)
      .map(x => scoreSingleAutocomplete(x.get))
      .toList
      .sorted

    scores(scores.size / 2)
  }

  private def scoreSingleAutocomplete(chars: List[Char]): BigInt = {
    var score = BigInt(0)
    for (c <- chars) {
      score = score * 5
      score = score + (c match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case '>' => 4
      })
    }

    score
  }
}

object InputParser {
  def parseInput(fileName: String): Array[NavigationLine] = {

    Using.resource(Source.fromFile(fileName)) { source =>
      source
        .getLines()
        .toArray
        .map(line => new NavigationLine(line))
    }
  }
}