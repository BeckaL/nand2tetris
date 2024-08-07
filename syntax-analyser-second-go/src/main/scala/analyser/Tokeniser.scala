package analyser

import scala.util.matching.Regex
import analyser.TokenTypes.*

trait Tokeniser {
  def advance(): Unit
  def currentToken: String
  def hasMoreTokens: Boolean
}

//At this point, input should be a string with only spaces as whitespace, newlines etc replaced by spaces
class DefaultTokeniser(input: String, var position: Int = 0, var currentToken: String = "") extends Tokeniser {
  def hasMoreTokens: Boolean =
    position != -1 && position < input.size && input.slice(position, input.size).exists(!_.isSpaceChar)

  def advance(): Unit = {
    val nextPosition = input.indexWhere(_ != ' ', position)
    if (input.indices.contains(nextPosition)) {
      val firstChar = input(nextPosition)
      val (regexToFind, offset) = terminatingRegexAndOffset(firstChar)
      val searchStart = if (nextPosition == input.indices.max) nextPosition else nextPosition + 1
      position = input.indexWhere(c => regexToFind.matches(c.toString), searchStart) + offset
      if (position == nextPosition && position == input.indices.max)
        position = position + 1
      currentToken = input.slice(nextPosition, position)
    } else {
      currentToken = ""
    }
  }

  def keyword: String = currentToken
  def symbol: Char = currentToken.charAt(0)
  def identifier: String = currentToken
  def intVal: Int = currentToken.toInt
  def stringVal: String = currentToken.tail.dropRight(1)

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



