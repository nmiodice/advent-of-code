import DisplayPattern.DisplayPattern

object DisplayUnJumbleDevice {
  def unJumbleDisplayGroup(group: DisplayGroup): List[Int] = {
    var possibilities: Map[DisplayPattern, Set[Int]] = group
      .diagnosticPatterns
      .map({ pattern => {
        (pattern -> Set(1, 2, 3, 4, 5, 6, 7, 8))
      }
      })
      .toMap

    for (p <- group.diagnosticPatterns) {
      possibilities = pairDownBasedOnSignalSize(p, possibilities)
    }

    possibilities = pairDownBasedOnSignalWires(possibilities)
    group
      .output
      .map({ pattern => possibilities(pattern).head })
      .toList
  }

  private def pairDownBasedOnSignalSize(pattern: DisplayPattern, currentPossibilities: Map[DisplayPattern, Set[Int]]): Map[DisplayPattern, Set[Int]] = {
    var newPossibilities = currentPossibilities
    pattern.signals.size match {
      // single possibility cases
      case 2 => newPossibilities = newPossibilities + (pattern -> Set(1))
      case 4 => newPossibilities = newPossibilities + (pattern -> Set(4))
      case 3 => newPossibilities = newPossibilities + (pattern -> Set(7))
      case 7 => newPossibilities = newPossibilities + (pattern -> Set(8))

      // multiple possibility cases
      case 5 => newPossibilities = newPossibilities + (pattern -> Set(2, 3, 5))
      case 6 => newPossibilities = newPossibilities + (pattern -> Set(0, 6, 9))
    }

    newPossibilities
  }

  // assumes 1, 4, 7 and 8 are already known
  private def pairDownBasedOnSignalWires(possibilities: Map[DisplayPattern, Set[Int]]): Map[DisplayPattern, Set[Int]] = {
    var updatedPossibilities = possibilities
    val p1 = possibilities.filter(e => e._2 == Set(1)).head._1
    val p4 = possibilities.filter(e => e._2 == Set(4)).head._1

    possibilities
      .filter(e => e._2.size > 1)
      .keys
      .foreach(p => {
        p.signals.size match {
          case 5 => {
            if (p.signals.intersect(p1.signals).size == 2) {
              updatedPossibilities = updatedPossibilities + (p -> Set(3))
            } else if (p.signals.intersect(p4.signals).size == 2) {
              updatedPossibilities = updatedPossibilities + (p -> Set(2))
            } else {
              updatedPossibilities = updatedPossibilities + (p -> Set(5))
            }
          }
          case 6 => {
            if (p.signals.intersect(p1.signals).size == 1) {
              updatedPossibilities = updatedPossibilities + (p -> Set(6))
            } else if (p.signals.intersect(p4.signals).size == 3) {
              updatedPossibilities = updatedPossibilities + (p -> Set(0))
            } else {
              updatedPossibilities = updatedPossibilities + (p -> Set(9))
            }
          }
        }
      })

    updatedPossibilities
  }
}


object Part1 extends App {
  val groups = InputParser.parseInputFile("input.txt")
  val unJumbledOutputs = groups.map(g => DisplayUnJumbleDevice.unJumbleDisplayGroup(g))
  val totalOf147And8 = unJumbledOutputs
    .flatMap({ output =>
      output
        .map({
          case 1 => 1
          case 4 => 1
          case 7 => 1
          case 8 => 1
          case _ => 0
        })
    })
    .sum

  println(totalOf147And8)
}

object Part2 extends App {
  val groups = InputParser.parseInputFile("input.txt")
  val unJumbledOutputs = groups.map(g => DisplayUnJumbleDevice.unJumbleDisplayGroup(g))
  val totalOfAllDigits = unJumbledOutputs
    .map(display => {
      display
        .map(x => x.toString)
        .mkString("")
        .toInt
    })
    .sum

  println(totalOfAllDigits)
}