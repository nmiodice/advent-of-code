
import scala.io.Source
import scala.util.Using

object InputParser {
  def parseInput(fileName: String): Array[Bit] = {
    Using.resource(Source.fromFile(fileName)) { source =>
      val input = source.getLines().next()
      input
        .toCharArray
        .flatMap(hexValue => Bit.fromHex(hexValue))
    }
  }
}

object Logic {
  def parsePacket(bits: Array[Bit]): Packet = {
    val packetType = Bit.toInt(bits.slice(3, 6))

    PacketType.fromInt(packetType) match {
      case PacketType.LITERAL =>
        val result = LiteralPacket.fromBits(bits)
        result._1
      case PacketType.OPERATOR =>
        val result = OperatorPacket.fromBits(bits)
        result._1
    }
  }
}

object Part1 extends App {
  val bits = InputParser.parseInput("input.txt")
  val packet = Logic.parsePacket(bits)
  println(packet)
}