import scala.collection.Map
import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val (template, insertions) = InputParser.parseInput("input.txt")
  val counts = Logic.constructPairCounts(template, insertions, 10)
  val charCountSpread = Logic.getCharacterSpread(template, counts)

  println(charCountSpread)
}

object Part2 extends App {
  val (template, insertions) = InputParser.parseInput("input.txt")
  val counts = Logic.constructPairCounts(template, insertions, 40)
  val charCountSpread = Logic.getCharacterSpread(template, counts)

  println(charCountSpread)
}

object Logic {
  def constructPairCounts(template: String, insertions: Map[(Char, Char), Char], rounds: Int): Map[(Char, Char), BigInt] = {
    var pairCounts: Map[(Char, Char), BigInt] = Map()

    // get initial pair counts from template
    for (i <- 0 until template.length - 1) {
      val pair = (template.charAt(i), template.charAt(i + 1))
      pairCounts = pairCounts + (pair -> (pairCounts.getOrElse(pair, BigInt(0)) + 1))
    }

    // process the rounds. we don't need to keep order, just track the number
    // of times a given pair shows up
    for (_ <- 0 until rounds) {
      var newCounts: Map[(Char, Char), BigInt] = Map()

      for ((pair, insertionChar) <- insertions) {

        // the pair was found, so we need to modify some counts...
        val pairEntry = pairCounts.get(pair)
        if (pairEntry.isDefined) {
          val leftPair = (pair._1, insertionChar)
          val rightPair = (insertionChar, pair._2)

          // the new left and right pairs will replace all the original pairs, and
          // a new pair is also being created
          newCounts = newCounts + (leftPair -> (newCounts.getOrElse(leftPair, BigInt(0)) + pairEntry.get))
          newCounts = newCounts + (rightPair -> (newCounts.getOrElse(rightPair, BigInt(0))  + pairEntry.get))
        }
      }
      pairCounts = newCounts.filter(entry => entry._2 > 0)
    }

    pairCounts
  }

  def getCharacterSpread(initialPolymer: String, pairCounts: Map[(Char, Char), BigInt]): BigInt = {
    var charCountsDoubled = Map[Char, BigInt]()

    // first and last elements are only a part of 1 pair each,
    // so we need to account for it by incrementing the count
    // of both by 1
    for (c <- List(initialPolymer.head, initialPolymer.last)) {
      charCountsDoubled = charCountsDoubled + (c -> 1)
    }

    // all other characters show up in 2 pairs each and so we'll intentionally double
    // count them
    pairCounts.foreach(entry => {
      val pair = entry._1
      val pairCount = entry._2
      charCountsDoubled = charCountsDoubled + (pair._1 -> (charCountsDoubled.getOrElse(pair._1, BigInt(0)) + pairCount))
      charCountsDoubled = charCountsDoubled + (pair._2 -> (charCountsDoubled.getOrElse(pair._2, BigInt(0)) + pairCount))
    })

    val sortedCounts = charCountsDoubled
      .values
      .toList
      .sorted

    // divide by two to get the non doubled answer
    (sortedCounts.last - sortedCounts.head) / 2
  }
}

object InputParser {
  def parseInput(fileName: String): (String, Map[(Char, Char), Char]) = {
    Using.resource(Source.fromFile(fileName)) { source =>
      var polymerTemplate = ""
      var insertions = Map[(Char, Char), Char]()

      for (line <- source.getLines()) {
        line.strip() match {
          case "" =>
          case it if (polymerTemplate == "") => polymerTemplate = it
          case it => {
            val parts = it.split("->").map(x => x.strip())
            insertions = insertions + ((parts(0)(0), parts(0)(1)) -> parts(1)(0))
          }
        }
      }

      (polymerTemplate, insertions)
    }
  }
}
