

object Part1 extends App {
  var ages = InputParser.getStateFromFile("input.txt")
  println(PopulationMonitor.runSimulationForDays(ages, 80))
}

object Part2 extends App {
  var ages = InputParser.getStateFromFile("input.txt")
  println(PopulationMonitor.runSimulationForDays(ages, 256))
}

object PopulationMonitor {
  def runSimulationForDays(initialAges: Map[Int, BigInt], days: Int): BigInt = {
    var ages: Map[Int, BigInt] = initialAges

    for (_ <- 0 until days) {
      var newAges: Map[Int, BigInt] = Map()
      for (age <- ages.keys) {
        if (age == 0) {
          newAges = newAges + (6 -> (ages(age) + newAges.getOrElse(6, 0)))
          newAges = newAges + (8 -> ages(age))
        } else {
          newAges = newAges + (age - 1 -> (ages(age) + newAges.getOrElse(age - 1, 0)))
        }
      }

      ages = newAges
    }

    ages.values.sum
  }
}