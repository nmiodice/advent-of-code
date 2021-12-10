import DisplayPattern.DisplayPattern

class DisplayGroup(val diagnosticPatterns: Seq[DisplayPattern], val output: Seq[DisplayPattern]) {
}

object SignalWire extends Enumeration {
  type SignalWire = Value
  val A, B, C, D, E, F, G = Value
}

object DisplayPattern {

  def fromString(s: String): DisplayPattern = {
    val signals: Set[SignalWire.SignalWire] = s
      .toLowerCase()
      .toCharArray
      .map {
        case 'a' => SignalWire.A
        case 'b' => SignalWire.B
        case 'c' => SignalWire.C
        case 'd' => SignalWire.D
        case 'e' => SignalWire.E
        case 'f' => SignalWire.F
        case 'g' => SignalWire.G
      }
      .toSet
    new DisplayPattern(signals)
  }

  class DisplayPattern(val signals: Set[SignalWire.SignalWire]) {
    override def equals(that: Any): Boolean = {
      that match {
        case that: DisplayPattern => signals.equals(that.signals)
        case _ => false
      }
    }

    override def hashCode: Int = {
      signals.hashCode()
    }
  }
}