package analyser

object InputUtils {
  def stripInput(input: List[String]): List[String] =
    removeComments(input.mkString("\n") ++ "\n").split("\n").toList.filter(line => line.trim != "")

  private def removeComments(input: String, count: Int = 0): String =
    val indexOfFirstMultiLineComment = input.indexOf("/*")
    val indexOfFirstCommentToEndOfLine = input.indexOf("//")

      (indexOfFirstMultiLineComment, indexOfFirstCommentToEndOfLine) match
        case (-1, -1) => input
        case (multiLineCommentStart, endOfLineCommentStart) =>
          val newString = if (multiLineCommentStart > 0 && (endOfLineCommentStart < 0 || multiLineCommentStart < endOfLineCommentStart))
            stripStringUntilNextClosingChar(input, multiLineCommentStart, "*/", 2) //offset is 0 because we want to strip closing "*/"
          else
            stripStringUntilNextClosingChar(input, endOfLineCommentStart, "\n", 0) //offset is 0 because we want to keep new lines
          removeComments(newString, count + 1)
  private def stripStringUntilNextClosingChar(string: String, startIndex: Int, closingString: String, offset: Int) =
    string.slice(startIndex, string.size).indexOf(closingString) match
      case -1 => throw new RuntimeException(s"Expected to find closing string ${closingString} in input ${string}")
      case n => string.slice(0, startIndex) ++ string.slice(startIndex + n + offset, string.size)
}
