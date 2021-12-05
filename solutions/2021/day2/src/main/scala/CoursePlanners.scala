trait CoursePlanner {
  def ApplyInstruction(instruction: String): Unit

  def depthPos(): Int

  def horizontalPos(): Int
}

class CoursePlannerPart1 extends CoursePlanner {

  private var _depthPos: Int = 0
  private var _horizontalPos: Int = 0

  def ApplyInstruction(instruction: String): Unit = {
    val parts = instruction.split(" ")
    val command = parts(0).toLowerCase
    val amount = parts(1).toInt

    command match {
      case "forward" => _horizontalPos = _horizontalPos + amount
      case "down" => _depthPos = _depthPos + amount
      case "up" => _depthPos = _depthPos - amount
    }
  }

  def depthPos(): Int = _depthPos

  def horizontalPos(): Int = _horizontalPos
}

class CoursePlannerPart2 extends CoursePlanner {
  private var _depthPos: Int = 0
  private var _horizontalPos: Int = 0
  private var _aim: Int = 0

  def ApplyInstruction(instruction: String): Unit = {
    val parts = instruction.split(" ")
    val command = parts(0).toLowerCase
    val amount = parts(1).toInt

    command match {
      case "forward" => {
        _horizontalPos = _horizontalPos + amount
        _depthPos = _depthPos + _aim * amount
      }
      case "down" => _aim = _aim + amount
      case "up" => _aim = _aim - amount
    }
  }

  def depthPos(): Int = _depthPos

  def horizontalPos(): Int = _horizontalPos
}