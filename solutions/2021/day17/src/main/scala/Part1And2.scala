import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

class Pair(val x: Int, val y: Int)

class PositionState(val position: Pair, val velocity: Pair) {
  def next: PositionState = {
    val nextXVelocity = velocity.x match {
      case x if x < 0 => x + 1
      case x if x > 0 => x - 1
      case _ => 0
    }
    val nextYVelocity = velocity.y - 1

    val nextXPosition = position.x + velocity.x
    val nextYPosition = position.y + velocity.y

    new PositionState(
      new Pair(nextXPosition, nextYPosition),
      new Pair(nextXVelocity, nextYVelocity)
    )
  }
}

class TargetArea(val minX: Int, val maxX: Int, val minY: Int, val maxY: Int)

object Part1 extends App {
  val targetArea = InputParser.parseInput("input.txt")
  val optimalState = Solver.optimizeForHeight(targetArea)

  if (optimalState.isDefined) {
    var state = optimalState.get
    println(s"optimal velocity found: (${state.velocity.x}, ${state.velocity.y})")

    while (state.velocity.y > 0) {
      state = state.next
    }

    println(state.position.y)
  } else {
    println("No answer found!")
    System.exit(-1)
  }
}

object Part2 extends App {
  val targetArea = InputParser.parseInput("input.txt")
  val states = Solver.findPossibleInitialStates(targetArea)
  println(states.size)
}

object Solver {
  def optimizeForHeight(target: TargetArea): Option[PositionState] = {
    var vY = math.abs(target.minY)
    while (vY >= target.minY) {
      for (vX <- 0 to target.maxX) {

        val initialState = new PositionState(
          new Pair(0, 0),
          new Pair(vX, vY))

        if (willHitTarget(target, initialState)) {
          return Some(initialState)
        }
      }

      vY = vY - 1
    }

    None
  }

  def findPossibleInitialStates(target: TargetArea): Set[PositionState] = {
    var states = Set[PositionState]()
    var vY = math.abs(target.minY)
    while (vY >= target.minY) {
      for (vX <- 0 to target.maxX) {
        val initialState = new PositionState(
          new Pair(0, 0),
          new Pair(vX, vY))
        if (willHitTarget(target, initialState)) {
          states = states + initialState
        }
      }

      vY = vY - 1
    }

    states
  }

  @tailrec
  def willHitTarget(targetArea: TargetArea, state: PositionState): Boolean = {
    state match {
      case s if isInTarget(targetArea, s) => true
      case s if isPastTarget(targetArea, s) => false
      case _ => willHitTarget(targetArea, state.next)
    }
  }

  private def isInTarget(targetArea: TargetArea, state: PositionState): Boolean = {
    val isInBoundsX = targetArea.minX <= state.position.x && targetArea.maxX >= state.position.x
    val isInBoundsY = targetArea.minY <= state.position.y && targetArea.maxY >= state.position.y

    isInBoundsX && isInBoundsY
  }

  private def isPastTarget(targetArea: TargetArea, state: PositionState): Boolean = {
    if (state.velocity.x > 0 && state.position.x > targetArea.maxX) {
      return true
    }

    if (state.velocity.x < 0 && state.position.x < targetArea.minY) {
      return true
    }

    if (state.velocity.y < 0 && state.position.y < targetArea.minY) {
      return true
    }

    false
  }
}

object InputParser {
  def parseInput(fileName: String): TargetArea = {
    Using.resource(Source.fromFile(fileName)) { source =>
      val line = source
        .getLines()
        .next()
        .strip()
        .replace(",", "")

      val xBits = line
        .split(' ')(2)
        .split('=')(1)
        .split("\\.\\.")
        .map(s => s.toInt)

      val yBits = line
        .split(' ')(3)
        .split('=')(1)
        .split("\\.\\.")
        .map(s => s.toInt)

      new TargetArea(
        xBits(0),
        xBits(1),
        yBits(0),
        yBits(1)
      )
    }
  }
}
