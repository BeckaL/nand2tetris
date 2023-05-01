package analyser

import scala.util.matching.Regex

class Tokeniser(input: String, var position: Int = 0) {

  def hasMoreTokens: Boolean =
    position < input.size && input.slice(position, input.size).exists(!_.isSpaceChar)


}



