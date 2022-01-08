object Tests extends App {
  InputParserTests.testParseInputLine()
  PairTests.testExplode()
  PairTests.testSplit()
  CalculatorTest.testAdd()
}

object CalculatorTest {
  def testAdd(): Unit = {
    val tests = List(
      List(
        "[[[[4,3],4],4],[7,[[8,4],9]]]",
        "[1,1]",
      ) -> "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]",
      List(
        "[1,1]",
        "[2,2]",
        "[3,3]",
        "[4,4]",
      ) -> "[[[[1,1],[2,2]],[3,3]],[4,4]]",
      List(
        "[1,1]",
        "[2,2]",
        "[3,3]",
        "[4,4]",
        "[5,5]"
      ) -> "[[[[3,0],[5,3]],[4,4]],[5,5]]",
      List(
        "[1,1]",
        "[2,2]",
        "[3,3]",
        "[4,4]",
        "[5,5]",
        "[6,6]",
      ) -> "[[[[5,0],[7,4]],[5,5]],[6,6]]",
      List(
        "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
        "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
        "[7,[5,[[3,8],[1,4]]]]",
        "[[2,[2,2]],[8,[8,1]]]",
        "[2,9]",
        "[1,[[[9,3],9],[[9,0],[0,7]]]]",
        "[[[5,[7,4]],7],1]",
        "[[[[4,2],2],6],[8,7]]",
      ) -> "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
    )

    for (t <- tests) {
      val expected = t._2
      val actual = Calculator.add(t._1.map(InputParser.parseInputLine): _*)
      assert(expected == actual.toString, s"\nexpected: $expected\ngot:      $actual")
    }
  }
}

object PairTests {
  def testExplode(): Unit = {
    val tests = Map[String, String](
      "[[[[[9,8],1],2],3],4]" -> "[[[[0,9],2],3],4]",
      "[7,[6,[5,[4,[3,2]]]]]" -> "[7,[6,[5,[7,0]]]]",
      "[[6,[5,[4,[3,2]]]],1]" -> "[[6,[5,[7,0]]],3]",
      "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" -> "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
      "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" -> "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",

    )

    for (t <- tests) {
      val pair = InputParser.parseInputLine(t._1)
      pair.tryExplode()
      val asString = pair.toString
      assert(t._2 == asString, f"expected ${t._2}, got $asString")
    }
  }

  def testSplit(): Unit = {
    val tests = Map[String, String](
      "[[[[0,7],4],[15,[0,13]]],[1,1]]" -> "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
      "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" -> "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
    )

    for (t <- tests) {
      val pair = InputParser.parseInputLine(t._1)
      pair.trySplit(pair)
      val asString = pair.toString
      assert(t._2 == asString, f"expected ${t._2}, got $asString")
    }
  }
}

object InputParserTests {
  def testParseInputLine(): Unit = {
    val tests = Map[String, Pair](
      "[1,2]" -> new Pair(
        new NumericComponent(1),
        new NumericComponent(2)
      ),
      "[20,30]" -> new Pair(
        new NumericComponent(20),
        new NumericComponent(30)
      ),
      "[[1,2],3]" -> new Pair(
        new Pair(
          new NumericComponent(1),
          new NumericComponent(2),
        ),
        new NumericComponent(3),
      ),
      "[9,[8,7]]" -> new Pair(
        new NumericComponent(9),
        new Pair(
          new NumericComponent(8),
          new NumericComponent(7),
        ),
      ),
      "[[1,9],[8,5]]" -> new Pair(
        new Pair(
          new NumericComponent(1),
          new NumericComponent(9),
        ),
        new Pair(
          new NumericComponent(8),
          new NumericComponent(5),
        ),
      ),
    )

    for (t <- tests) {
      val pair = InputParser.parseInputLine(t._1)
      assert(t._2.toString == pair.toString, f"expected ${t._2} but got $pair")
    }
  }
}