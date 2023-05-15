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
    position = input.indexWhere(c => terminatingRegex(firstChar).matches(c.toString), nextPosition + 1)
    currentToken = input.slice(nextPosition, position)
  }

  private def terminatingRegex(firstChar: Char): Regex =
    if (TokenTypes.symbols.contains(firstChar.toString))
      ".".r
    else if (firstChar.isDigit)
      "[^\\d]".r
    else
      "[^a-zA-Z_]".r
}



