
import scala.io.Source
import scala.util.Using

object InputParser {
  def parseInput(fileName: String): Array[Bit] = {
    Using.resource(Source.fromFile(fileName)) { source =>
      parseInputString(source.getLines().next())
    }
  }

  def parseInputString(input: String): Array[Bit] = {
    input
      .toCharArray
      .flatMap(hexValue => Bit.fromHex(hexValue))
  }
}

object Part1 extends App {
  val bits = InputParser.parseInput("input.txt")
  val results = Packet.parsePacket(bits)
  val versions = PacketFlattener.getVersions(results._1)
  println(versions.sum)
}

object Part2 extends App {
  val bits = InputParser.parseInput("input.txt")
  val evaluation = Packet.parsePacket(bits)._1.evaluate()
  println(evaluation)
}

object PacketFlattener {
  def getVersions(rootPacket: Packet): Array[Int] = {
    rootPacket match {
      case it: LiteralPacket => Array(it.pVersion)
      case it: OperatorPacket => it.packets.flatMap(p => getVersions(p)) :+ it.pVersion
    }
  }
}

object ExamplesFromProblemStatement extends App {
  val results1 = Packet.parsePacket(InputParser.parseInputString("D2FE28"))
  val packet1 = results1._1
  assert(packet1.isInstanceOf[LiteralPacket])
  assert(packet1.asInstanceOf[LiteralPacket].pVersion == 6)
  assert(packet1.asInstanceOf[LiteralPacket].pValue == 2021)

  val remainingBits1 = results1._2
  assert(remainingBits1.length == 3)


  val results2 = Packet.parsePacket(InputParser.parseInputString("38006F45291200"))
  val packet2 = results2._1
  assert(packet2.isInstanceOf[OperatorPacket])
  assert(packet2.asInstanceOf[OperatorPacket].pVersion == 1)
  assert(packet2.asInstanceOf[OperatorPacket].packets.length == 2)

  val remainingBits2 = results2._2
  assert(remainingBits2.length == 7)

  val results3 = Packet.parsePacket(InputParser.parseInputString("EE00D40C823060"))
  val packet3 = results3._1
  assert(packet3.isInstanceOf[OperatorPacket])
  assert(packet3.asInstanceOf[OperatorPacket].pVersion == 7)
  assert(packet3.asInstanceOf[OperatorPacket].packets.length == 3)

  val remainingBits3 = results3._2
  assert(remainingBits3.length == 5)

  val inputToVersionCounts = Map(
    ("8A004A801A8002F478" -> 16),
    ("620080001611562C8802118E34" -> 12),
    ("C0015000016115A2E0802F182340" -> 23),
    ("A0016C880162017C3686B18A3D4780" -> 31),
  )

  inputToVersionCounts.foreach(entry => {
    val packet = Packet.parsePacket(InputParser.parseInputString(entry._1))._1
    val count = PacketFlattener.getVersions(packet)

    assert(count.sum == entry._2)
  })

  val inputToEvaluations = Map(
    ("C200B40A82" -> 3),
    ("04005AC33890" -> 54),
    ("880086C3E88112" -> 7),
    ("CE00C43D881120" -> 9),
    ("D8005AC2A8F0" -> 1),
    ("F600BC2D8F" -> 0),
    ("9C005AC2F8F0" -> 0),
    ("9C0141080250320F1802104A08" -> 1)
  )

  inputToEvaluations.foreach(entry => {
    val packet = Packet.parsePacket(InputParser.parseInputString(entry._1))._1
    assert(packet.evaluate() == entry._2)
  })
}