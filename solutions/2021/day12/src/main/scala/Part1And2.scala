import scala.io.Source
import scala.util.Using

class PathFindingCriterion[
  TargetType,
  StateType
](
   val graph: Graph[TargetType],
   val current: TargetType,
   val target: TargetType,
   val onVisit: (TargetType, StateType) => StateType,
   val traversableNodes: (TargetType, StateType, Graph[TargetType]) => List[TargetType],
   val state: StateType
 ) {}

case class Part2State(visitedCount: Map[Cave, Int]) {}

object Part1 extends App {
  val graph = InputParser.parseGraph("input.txt")
  val start = new Cave("start")
  val end = new Cave("end")

  val onVisit = (cave: Cave, visited: Set[Cave]) => {
    visited + cave
  }

  val traversableNodes = (cave: Cave, visited: Set[Cave], graph: Graph[Cave]) => {
    graph.edgesFor(cave)
      .filter(c => !visited.contains(c) || !c.isSmallCave)
      .toList
  }

  val searchCriterion = new PathFindingCriterion[Cave, Set[Cave]](
    graph,
    start,
    end,
    onVisit,
    traversableNodes,
    Set()
  )

  val paths = Logic.findPaths(searchCriterion)
  println(paths.size)
}

object Part2 extends App {
  val graph = InputParser.parseGraph("input.txt")
  val start = new Cave("start")
  val end = new Cave("end")

  val onVisit = (cave: Cave, state: Part2State) => {
    Part2State(
      state.visitedCount + (
        cave -> (state.visitedCount.getOrElse(cave, 0) + 1)
        )
    )
  }

  val traversableNodes = (cave: Cave, state: Part2State, graph: Graph[Cave]) => {
    // identify possible next paths
    graph
      .edgesFor(cave)
      .filter(c => {
        var canVisit = false

        if (state.visitedCount.getOrElse(c, 0) == 0) {
          // an unvisited node can be visited
          canVisit = true
        } else if (!c.isSmallCave) {
          // a large cave can be visited any number of times
          canVisit = true
        } else if (c == start || c == end) {
          // start and end can only be visited once
          canVisit = state.visitedCount.getOrElse(c, 0) == 0
        } else {
          // only one small cave can be visited more than once
          val smallCavesVisitedTwice = state
            .visitedCount
            .filter(countEntry => countEntry._1.isSmallCave)
            .filter(countEntry => countEntry._2 == 2)

          canVisit = smallCavesVisitedTwice.isEmpty
        }
        canVisit
      })
      .toList
  }

  val searchCriterion = new PathFindingCriterion[Cave, Part2State](
    graph,
    start,
    end,
    onVisit,
    traversableNodes,
    Part2State(Map())
  )

  val paths = Logic.findPaths(searchCriterion)
  println(paths.size)
}

object Logic {
  def findPaths[T, B](criterion: PathFindingCriterion[T, B]): Set[Vector[T]] = {
    val newState = criterion.onVisit(criterion.current, criterion.state)
    var validPaths: Set[Vector[T]] = Set()

    criterion.current match {
      case _ if criterion.current == criterion.target =>
        validPaths = validPaths + Vector(criterion.target)
      case _ =>
        criterion
          .traversableNodes(criterion.current, newState, criterion.graph)
          .foreach(cave => {
            val nextCriterion = new PathFindingCriterion[T, B](
              criterion.graph,
              cave,
              criterion.target,
              criterion.onVisit,
              criterion.traversableNodes,
              newState
            )
            findPaths(nextCriterion) match {
              case it if it.isEmpty =>
              case it => validPaths = validPaths ++ it.map(path => {
                path.prepended(criterion.current)
              })
            }
          })
    }
    validPaths
  }
}

object InputParser {
  def parseGraph(fileName: String): Graph[Cave] = {
    val g = new Graph[Cave]()
    Using.resource(Source.fromFile(fileName)) { source =>
      for (line <- source.getLines()) {
        val parts = line.strip().split('-').map(x => x.strip())
        g.addBiDirectionalEdge(new Cave(parts(0)), new Cave(parts(1)))
      }
    }
    g
  }
}
