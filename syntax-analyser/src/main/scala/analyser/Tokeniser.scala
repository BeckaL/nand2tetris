package analyser

import scala.util.matching.Regex

class Tokeniser(input: String, var position: Int = 0, var currentToken: String = "") {

  def hasMoreTokens: Boolean =
    position < input.size && input.slice(position, input.size).exists(!_.isSpaceChar)

  def advance: Unit = {
    ()
  }

}



