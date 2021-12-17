class Bit private (isFlipped: Boolean) {
  override def toString: String = if (isFlipped) {
    "1"
  } else {
    "0"
  }
}

object Bit {

  val ZERO: Bit = new Bit(false)
  val ONE: Bit = new Bit(true)

  def fromHex(c: Char): Array[Bit] = {
    c match {
      case '0' => Array(ZERO, ZERO, ZERO, ZERO)
      case '1' => Array(ZERO, ZERO, ZERO, ONE)
      case '2' => Array(ZERO, ZERO, ONE, ZERO)
      case '3' => Array(ZERO, ZERO, ONE, ONE)
      case '4' => Array(ZERO, ONE, ZERO, ZERO)
      case '5' => Array(ZERO, ONE, ZERO, ONE)
      case '6' => Array(ZERO, ONE, ONE, ZERO)
      case '7' => Array(ZERO, ONE, ONE, ONE)
      case '8' => Array(ONE, ZERO, ZERO, ZERO)
      case '9' => Array(ONE, ZERO, ZERO, ONE)
      case 'A' => Array(ONE, ZERO, ONE, ZERO)
      case 'B' => Array(ONE, ZERO, ONE, ONE)
      case 'C' => Array(ONE, ONE, ZERO, ZERO)
      case 'D' => Array(ONE, ONE, ZERO, ONE)
      case 'E' => Array(ONE, ONE, ONE, ZERO)
      case 'F' => Array(ONE, ONE, ONE, ONE)
    }
  }

  def toInt(bits: Array[Bit]): Int = {
    var sum = 0
    for (x <- bits.reverse.zipWithIndex) {
      val bit = x._1
      val index = x._2
      bit match {
        case ONE => sum = sum + (1 << index)
        case ZERO =>
      }
    }

    sum
  }
}