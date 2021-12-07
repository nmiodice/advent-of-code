object Part1 extends App {
  val (draws, boards) = InputParser.parse("input.txt")
  val boardIndex = new BoardIndex(boards)

  var winner: Option[Board] = Option.empty
  var round = 0
  var draw = 0

  while (winner.isEmpty) {
    draw = draws(round)
    val boardWithNumber = boardIndex.boardsWithNumber(draw)

    boardWithNumber.foreach(board => {
      board.mark(draw)
      if (board.isWinner) {
        winner = Option(board)
      }
    })

    round = round + 1
  }

  if (winner.isEmpty) {
    println("Did not find winner...?!?!")
  } else {
    println(winner.get.getUnmarkedNumbers.sum * draw)
  }
}
