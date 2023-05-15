package analyser

import scala.util.matching.Regex
import analyser.TokenTypes.*

//At this point, input should be a string with only spaces as whitespace, newlines etc replaced by spaces
class Tokeniser(input: String, var position: Int = 0, var currentToken: String = "") {

  def hasMoreTokens: Boolean =
    position < input.size && input.slice(position, input.size).exists(!_.isSpaceChar)

  def advance(): Unit = {
    val nextPosition = input.indexWhere(_ != ' ', position)
    val firstChar = input(nextPosition)
    val (regexToFind, offset) = terminatingRegexAndOffset(firstChar)
    position = input.indexWhere(c => regexToFind.matches(c.toString), nextPosition + 1) + offset
    currentToken = input.slice(nextPosition, position)
  }

  private def terminatingRegexAndOffset(firstChar: Char): (Regex, Int) =
    if (TokenTypes.symbols.contains(firstChar.toString))
      (".".r, 0)
    else if (firstChar == '"')
      ("\"".r, 1) //offset is bigger here because we want to include the closing double quote in the string
    else if (firstChar.isDigit)
      ("[^\\d]".r, 0)
    else
      ("[^a-zA-Z_]".r, 0)
}



