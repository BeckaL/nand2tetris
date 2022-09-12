package assembler

import scala.util.matching.Regex
import utils.implicits._
import Constants._

object Assembler {
  val VariablePattern: Regex = "@([A-Za-z][\\S]*)".r
  val ConstantPattern: Regex = "@([0-9]+)".r
  val SymbolPattern: Regex = "\\(([A-Za-z][\\S]*)\\)".r

  def assemble(instructions: List[String]): List[String] = parseAsBinaryString(instructions, getMap(instructions))

  def parseAsBinaryString(instructions: List[String], symbolsMap: Map[String, Int]): List[String] =
    instructions.map(_.stripCommentsAndWhitespace).filterNot(_.isEmpty).map {
      case VariablePattern(variable) => Some(symbolsMap(variable).toPaddedBinaryString(16))
      case ConstantPattern(variable) => Some(variable.toPaddedBinaryString(16))
      case SymbolPattern(_) => None
      case cInstructionString => Some(cInstruction(cInstructionString.mkString("")))
    }.collect { case Some(binString) => binString }

  def getMap(instructions: List[String]): Map[String, Int] =
    val trimmedInstructions = instructions.map(_.stripCommentsAndWhitespace).filterNot(_.isEmpty)
    val symbolsAfterFirstPass = parseSymbols(trimmedInstructions.filterNot(_.isEmpty).zipWithIndex, Map())
    parseVariables(trimmedInstructions, PREDEFINED_SYMBOLS ++ symbolsAfterFirstPass)

  private def parseVariables(instructions: List[String], mapWithSymbols: Map[String, Int]): Map[String, Int] =
    instructions.foldLeft((mapWithSymbols, 16)) { case ((map, index), instruction) =>
      instruction match
        case VariablePattern(variable) if !map.contains(variable) => (map.updated(variable, index), index + 1)
        case _ => (map, index)
    }._1

  private def parseSymbols(filteredInstructionsWithIndex: List[(String, Int)], currentMap: Map[String, Int]): Map[String, Int] =
    filteredInstructionsWithIndex match {
      case Nil => currentMap
      case (firstInstruction, index) :: otherInstructions =>
        firstInstruction match {
          case SymbolPattern(variable) =>
            val newInstructionsWithIndex = otherInstructions.map { case (str, i) => (str, i - 1) }
            val newMap = currentMap.updated(variable, index)
            parseSymbols(newInstructionsWithIndex, newMap)
          case _ => parseSymbols(otherInstructions, currentMap)
        }
    }

  private def cInstruction(string: String): String =
    val (comp, maybeJump) = string.splitIntoMaxTwo(";")
    val (compBits, destBits) = comp.splitIntoMaxTwo("=") match {
      case (storeIn, Some(computation)) => (computationBits(computation), destinationBits(storeIn))
      case (computation, None) => (computationBits(computation), destinationBits(""))
    }
    "111" + compBits + destBits + jumpBits(maybeJump)

  private def computationBits(string: String): String =
    string.stripCommentsAndWhitespace match {
      case mString if mString.contains("M") => "1" + COMPUTATION_BITS_MAP(mString.replace("M", "A"))
      case aString => "0" + COMPUTATION_BITS_MAP(aString)
    }

  private def destinationBits(string: String): String =
    List(string.contains("A"), string.contains("D"), string.contains("M")).map { b => if (b) '1' else '0' }.mkString("")

  private def jumpBits(jumpString: Option[String]): String =
    JUMP_BITS_MAP(jumpString.map(_.stripCommentsAndWhitespace))
}
