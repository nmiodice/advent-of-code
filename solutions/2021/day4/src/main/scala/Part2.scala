object Part2 extends App {
  val (draws, boards) = InputParser.parse("input.txt")
  val boardIndex = new BoardIndex(boards)

  var winners: Array[Board] = Array.empty
  var round = 0
  var lastWinningDraw = 0

  while (round != draws.length) {
    val draw = draws(round)
    val boardWithNumber = boardIndex.boardsWithNumber(draw)

    boardWithNumber.foreach(board => {
      // skip previous winners
      if (!board.isWinner) {
        board.mark(draw)
        if (board.isWinner) {
          winners = winners :+ board
          lastWinningDraw = draw
        }
      }
    })

    round = round + 1
  }

  if (winners.isEmpty) {
    println("Did not find winner...?!?!")
  } else {
    println(winners.last.getUnmarkedNumbers.sum * lastWinningDraw)
  }
}
