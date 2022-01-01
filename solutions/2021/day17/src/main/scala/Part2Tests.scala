object Part2Tests extends App {
  val testTarget = new TargetArea(20, 30, -10, -5)
  val possibleStates = Solver.findPossibleInitialStates(testTarget)

  assert(possibleStates.size == 112, "incorrect number of possible states!")
}
