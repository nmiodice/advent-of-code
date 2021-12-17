import PacketType.PacketType

trait Packet {
  val pType: PacketType.PacketType
  val pVersion: Int
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

class LiteralPacket(val pVersion: Int, val pValue: Int) extends Packet {
  val pType: PacketType = PacketType.LITERAL
  override def toString: String = f"LiteralPacket(version=$pVersion, value=$pValue)"
}

object LiteralPacket {
  def fromBits(bits: Array[Bit]): (Packet, Array[Bit]) = {
    val packetVersion = Bit.toInt(bits.slice(0, 3))

    var literalComponentBits: Array[Bit] = Array()
    var i = 6
    var continue = true
    while (continue) {
      continue = bits(i) == Bit.ONE
      literalComponentBits = literalComponentBits ++ bits.slice(i + 1, i + 5)
      i = i + 5
    }

    val packet = new LiteralPacket(packetVersion, Bit.toInt(literalComponentBits))
    val remainingBits = bits.slice(i, bits.length)

    (packet, remainingBits)
  }
}

class OperatorPacket(val pVersion: Int) extends Packet {
  val pType: PacketType = PacketType.OPERATOR
  override def toString: String = f"OperatorPacket(version=$pVersion)"
}

object OperatorPacket {
  def fromBits(bits: Array[Bit]): (Packet, Array[Bit]) = {
    val result = packetsFromBits(bits)
    assert(result._1.length == 1)
    (result._1(0), result._2)
  }

  private def packetsFromBits(bits: Array[Bit]): (Array[Packet], Array[Bit]) = {
    val packetVersion = Bit.toInt(bits.slice(0, 3))
    (Array(new OperatorPacket(packetVersion)), Array[Bit]())
  }
}