import scala.collection.mutable

class NavigationLine(private val line: String) {

  def firstSyntaxError: Option[Char] = {
    val (_, invalidChar) = syntaxChecker
    invalidChar
  }

  def autoComplete: Option[List[Char]] = {
    val (chunksStack, invalidChar) = syntaxChecker
    if (invalidChar.isDefined) {
      return None
    }

    var completionChars = List[Char]()
    while (chunksStack.nonEmpty) {
      completionChars = completionChars :+ NavigationLine.CLOSERS(chunksStack.pop())
    }

    Some(completionChars)
  }

  // first item returned is the fully processed stack
  // second item returned is a syntax error, if one was found
  private def syntaxChecker: (mutable.Stack[Char], Option[Char]) = {
    val chunks = mutable.Stack[Char]()
    line.toCharArray.foreach(c => {
      if (NavigationLine.OPENERS.contains(c)) {
        chunks.push(c)
      } else {
        val expectedCloser = NavigationLine.CLOSERS(chunks.top)
        if (expectedCloser == c) {
          chunks.pop()
        } else {
          return (chunks, Some(c))
        }
      }
    })
    (chunks, None)
  }

  private object NavigationLine {
    val OPENERS = Set('(', '[', '<', '{')
    val CLOSERS: Map[Char, Char] = Map(
      '(' -> ')',
      '[' -> ']',
      '<' -> '>',
      '{' -> '}',
    )
  }
}
