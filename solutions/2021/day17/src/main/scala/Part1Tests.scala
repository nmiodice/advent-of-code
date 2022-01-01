object Part1Tests extends App {
  val testTarget = new TargetArea(20, 30, -10, -5)
  val tests = Map[PositionState, Boolean](
    new PositionState(new Pair(0, 0), new Pair(7, 2)) -> true,
    new PositionState(new Pair(0, 0), new Pair(6, 3)) -> true,
    new PositionState(new Pair(0, 0), new Pair(9, 0)) -> true,
    new PositionState(new Pair(0, 0), new Pair(6, 9)) -> true,
    new PositionState(new Pair(0, 0), new Pair(17, -4)) -> false,
  )

  for (t <- tests) {
    val willHit = Solver.willHitTarget(testTarget, t._1)
    val shouldHit = t._2

    assert(
      willHit == shouldHit,
      s"unexpected result: velocity (${t._1.velocity.x}, ${t._1.velocity.y}), willHit: $willHit, shouldHit: ${t._2}"
    )
  }
}