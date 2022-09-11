import utils.StringOps

object Lexer {
  def getMap(instructions: List[String]): Map[String, Int] =
    val trimmedInstructions = instructions.map(_.stripCommentsAndWhitespace).filterNot(_.isEmpty)
    val symbolsAfterFirstPass = PREDEFINED_SYMBOLS ++ parseSymbols(trimmedInstructions.filterNot(_.isEmpty).zipWithIndex, Map())
    parseVariables(trimmedInstructions, symbolsAfterFirstPass)

  private def parseVariables(instructions: List[String], mapWithSymbols: Map[String, Int]): Map[String, Int] =
    instructions.foldLeft((mapWithSymbols, 16)) { case ((map, index), instruction) =>
      val VariablePattern = "@([A-Za-z]+)".r
      instruction match
        case VariablePattern(variable) if !map.contains(variable) => (map.updated(variable, index), index + 1)
        case _ => (map, index)
    }._1

  private def parseSymbols(filteredInstructionsWithIndex: List[(String, Int)], currentMap: Map[String, Int]): Map[String, Int] =
    filteredInstructionsWithIndex match {
      case Nil => currentMap
      case (firstInstruction, index) :: otherInstructions =>
        val SymbolPattern = "\\(([A-Za-z]+)\\)".r
        firstInstruction match {
          case SymbolPattern(variable) =>
            val newInstructionsWithIndex = otherInstructions.map { case (str, i) => (str, i - 1) }
            val newMap = currentMap.updated(variable, index)
            parseSymbols(newInstructionsWithIndex, newMap)
          case _ => parseSymbols(otherInstructions, currentMap)
        }
    }


  val PREDEFINED_SYMBOLS = Map[String, Int](
    "SP" -> 0,
    "LCL" -> 1,
    "ARG" -> 2,
    "THIS" -> 3,
    "THAT" -> 4,
    "SCREEN" -> 16384,
    "KBD" -> 24576,
  ) ++ (0 to 15).map(i => s"R$i" -> i).toMap


}
