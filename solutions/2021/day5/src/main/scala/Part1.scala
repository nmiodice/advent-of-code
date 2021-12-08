import scala.collection.mutable

object Part1 extends App {
  val lines = InputParser
    .parse("input.txt")
    .filter(line => line.isVertical || line.isHorizontal)

  val coverageCountMap: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()
  var totalWithMoreThanTwo = 0

  lines.foreach(line => {
    line.covers.foreach(point => {
      val newCount = coverageCountMap.getOrElse(point, 0) + 1
      coverageCountMap.put(point, newCount)

      if (newCount == 2) {
        totalWithMoreThanTwo = totalWithMoreThanTwo + 1
      }
    })
  })

  println(totalWithMoreThanTwo)
}
