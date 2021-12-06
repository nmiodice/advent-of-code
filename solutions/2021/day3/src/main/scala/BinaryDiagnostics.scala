import scala.collection.mutable

class BinaryDiagnostics {

  val powerTool = new PowerDiagnosticTool()
  val oxygenTool = new OxygenGeneratorTool()

  def ParseDiagnosticLine(line: String): Unit = {
    powerTool.ParseDiagnosticLine(line)
    oxygenTool.ParseDiagnosticLine(line)
  }

  def powerUsage(): Int = {
    powerTool.epsilon() * powerTool.gamma()
  }

  def lifeSupportRating(): Int = {
    oxygenTool.oxygenRating() * oxygenTool.co2Rating()
  }

  private def binaryStringToInt(binString: String): Int = {
    var sum = 0
    for (i <- 0 until binString.length) {
      val binChar = binString.charAt(i)
      if (binChar == '1') {
        sum = sum + (1 << (binString.length - i - 1))
      }
    }
    sum
  }

  class OxygenGeneratorTool {
    private val readingsSet: mutable.Set[String] = mutable.Set[String]()

    def ParseDiagnosticLine(line: String): Unit = {
      readingsSet.add(line)
    }

    def oxygenRating(): Int = {
      var oxygenCandidates = readingsSet.clone()
      var index = 0

      while (oxygenCandidates.size != 1) {
        val zeros = mutable.Set[String]()
        val ones = mutable.Set[String]()

        for (reading <- oxygenCandidates.iterator) {
          reading.charAt(index) match {
            case '0' => zeros.add(reading)
            case '1' => ones.add(reading)
          }
        }

        if (ones.size >= zeros.size) {
          oxygenCandidates = ones
        } else {
          oxygenCandidates = zeros
        }
        index = index + 1
      }

      binaryStringToInt(oxygenCandidates.head)
    }

    def co2Rating(): Int = {
      var co2Candidates = readingsSet.clone()
      var index = 0

      while (co2Candidates.size != 1) {
        val zeros = mutable.Set[String]()
        val ones = mutable.Set[String]()

        for (reading <- co2Candidates.iterator) {
          reading.charAt(index) match {
            case '0' => zeros.add(reading)
            case '1' => ones.add(reading)
          }
        }

        if (zeros.size <= ones.size) {
          co2Candidates = zeros
        } else {
          co2Candidates = ones
        }
        index = index + 1
      }

      binaryStringToInt(co2Candidates.head)
    }
  }

  class PowerDiagnosticTool {
    private val binaryDigitCounts = mutable.HashMap[Int, (Int, Int)]()
    private var maxBinaryLineLength = 0

    def ParseDiagnosticLine(line: String): Unit = {
      val lineLength = line.length()
      maxBinaryLineLength = math.max(maxBinaryLineLength, lineLength)

      for (i <- 0 until lineLength) {
        if (!binaryDigitCounts.contains(i)) {
          binaryDigitCounts.put(i, (0, 0))
        }

        val currentCount = binaryDigitCounts(i)

        line.charAt(i) match {
          case '0' => binaryDigitCounts.put(i, (currentCount._1 + 1, currentCount._2))
          case '1' => binaryDigitCounts.put(i, (currentCount._1, currentCount._2 + 1))
          case _ => println("wut")
        }
      }
    }

    def gamma(): Int = {
      val binaryString = new StringBuilder()
      for (i <- 0 until maxBinaryLineLength) {
        val counts = binaryDigitCounts(i)
        if (counts._1 > counts._2) {
          binaryString.append('0')
        } else {
          binaryString.append('1')
        }
      }

      binaryStringToInt(binaryString.toString())
    }

    def epsilon(): Int = {
      val binaryString = new StringBuilder()
      for (i <- 0 until maxBinaryLineLength) {
        val counts = binaryDigitCounts(i)
        if (counts._1 > counts._2) {
          binaryString.append('1')
        } else {
          binaryString.append('0')
        }
      }

      binaryStringToInt(binaryString.toString())
    }
  }
}
