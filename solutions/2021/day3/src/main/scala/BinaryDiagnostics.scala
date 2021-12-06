import scala.collection.mutable

class BinaryDiagnostics {
  private var binaryDigitCounts = mutable.HashMap[Int, (Int, Int)]()
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
}
