import PacketType.PacketType

trait Packet {
  val pType: PacketType.PacketType
  val pVersion: Int

  def evaluate(): BigInt
}

class LiteralPacket(val pVersion: Int, val pValue: BigInt) extends Packet {
  val pType: PacketType = PacketType.LITERAL

  override def evaluate(): BigInt = pValue
}

class OperatorPacket(val pVersion: Int, val typeID: Int, val packets: Array[Packet]) extends Packet {
  val pType: PacketType = PacketType.OPERATOR

  override def evaluate(): BigInt = {
    val subPacketEvaluations = packets.map(p => p.evaluate())
    typeID match {
      case 0 =>
        subPacketEvaluations.sum
      case 1 =>
        subPacketEvaluations.product
      case 2 =>
        subPacketEvaluations.min
      case 3 =>
        subPacketEvaluations.max
      case 5 =>
        if (subPacketEvaluations(0) > subPacketEvaluations(1)) 1 else 0
      case 6 =>
        if (subPacketEvaluations(0) < subPacketEvaluations(1)) 1 else 0
      case 7 =>
        if (subPacketEvaluations(0) == subPacketEvaluations(1)) 1 else 0
    }
  }
}

object Packet {
  def parsePacket(bits: Array[Bit]): (Packet, Array[Bit]) = {
    val packetType = Bit.toBigInt(bits.slice(3, 6)).toInt

    PacketType.fromInt(packetType) match {
      case PacketType.LITERAL =>
        literalPacketFromBits(bits)
      case PacketType.OPERATOR =>
        operatorPacketFromBits(bits)
    }
  }

  private def literalPacketFromBits(bits: Array[Bit]): (Packet, Array[Bit]) = {
    val packetVersion = Bit.toBigInt(bits.slice(0, 3)).toInt

    var literalComponentBits: Array[Bit] = Array()
    var i = 6
    var continue = true
    while (continue) {
      continue = bits(i) == Bit.ONE
      literalComponentBits = literalComponentBits ++ bits.slice(i + 1, i + 5)
      i = i + 5
    }

    val packet = new LiteralPacket(packetVersion, Bit.toBigInt(literalComponentBits))
    val remainingBits = bits.slice(i, bits.length)

    (packet, remainingBits)
  }

  private def operatorPacketFromBits(bits: Array[Bit]): (Packet, Array[Bit]) = {
    val packetVersion = Bit.toBigInt(bits.slice(0, 3)).toInt
    val packetType = Bit.toBigInt(bits.slice(3, 6)).toInt
    var subPackets = Array[Packet]()
    var remainingBits: Array[Bit] = Array()

    bits(6) match {
      case Bit.ZERO =>
        val bitsInSubPackets = Bit.toBigInt(bits.slice(7, 7 + 15))
        val expectedRemaining = bits.length - bitsInSubPackets - 7 - 15

        remainingBits = bits.slice(7 + 15, bits.length)
        while (remainingBits.length != expectedRemaining) {
          val subPacketResults = parsePacket(remainingBits)
          remainingBits = subPacketResults._2
          subPackets = subPackets :+ subPacketResults._1
        }
      case Bit.ONE =>
        val subPacketCount = Bit.toBigInt(bits.slice(7, 7 + 11)).toInt
        remainingBits = bits.slice(7 + 11, bits.length)

        for (_ <- 0 until subPacketCount) {
          val subPacketResults = parsePacket(remainingBits)
          remainingBits = subPacketResults._2
          subPackets = subPackets :+ subPacketResults._1
        }
    }

    (new OperatorPacket(packetVersion, packetType, subPackets), remainingBits)
  }
}

object PacketType extends Enumeration {
  type PacketType = Value
  val LITERAL, OPERATOR = Value

  def fromInt(i: Int): PacketType = {
    i match {
      case 4 => LITERAL
      case _ => OPERATOR
    }
  }
}
