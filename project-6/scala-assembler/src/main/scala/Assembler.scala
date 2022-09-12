import utils.{IntOps, StringOps}

object Assembler {
  def parseAsBinaryString(instructions: List[String], symbolsMap: Map[String, Int]): List[String] =
    instructions.map(str =>
      str.trim.toList match {
        case '@' :: rest => Some(aInstruction(rest.mkString(""), symbolsMap))
        case '/' :: '/' :: _ => None
        case '(' :: _ => None
        case Nil => None
        case cInstructionString => Some(cInstruction(cInstructionString.mkString("")))
      }
    ).collect{ case Some(binString) => binString}

  private def aInstruction(string: String, symbolsMap: Map[String, Int]): String = {
    val trimmedString = string.stripCommentsAndWhitespace
    symbolsMap.get(trimmedString) match {
      case Some(i) => "0" + i.toPaddedBinaryString(15)
      case None => "0" + trimmedString.toPaddedBinaryString(15)
    }
  }

  private def cInstruction(string: String): String = {
    val (comp, maybeJump) = string.splitIntoMaxTwo(";")
    val (compBits, destBits) = comp.splitIntoMaxTwo("=") match {
      case (storeIn, Some(computation)) => (computationBits(computation), destinationBits(storeIn))
      case (computation, None) => (computationBits(computation), destinationBits(""))
    }
    val res = "111" + compBits + destBits + jumpBits(maybeJump)
    res
  }

  private def computationBits(string: String): String =
    string.stripCommentsAndWhitespace match {
      case mString if mString.contains("M") => "1" + COMPUTATION_BITS_MAP(mString.replace("M", "A"))
      case aString => "0" + COMPUTATION_BITS_MAP(aString)
    }

  private def destinationBits(string: String): String =
    List(string.contains("A"), string.contains("D"), string.contains("M")).map{ b => if (b) '1' else '0' }.mkString("")

  private def jumpBits(jumpString: Option[String]): String =
    JUMP_BITS_MAP(jumpString.map(_.stripCommentsAndWhitespace))

  val COMPUTATION_BITS_MAP = Map(
    "0" -> "101010",
    "1" -> "111111",
    "-1" -> "111010",
    "D" -> "001100",
    "A" -> "110000",
    "!D" -> "001101",
    "!A" -> "110001",
    "-D" -> "001111",
    "-A" -> "110011",
    "D+1" -> "011111",
    "A+1" -> "110111",
    "D-1" -> "001110",
    "A-1" -> "110010",
    "D+A" -> "000010",
    "D-A" -> "010011",
    "A-D" -> "000111",
    "D&A" -> "000000",
    "D|A" -> "010101"
  )

  val JUMP_BITS_MAP = Map(
    None -> "000",
    Some("JGT") -> "001",
    Some("JEQ") -> "010",
    Some("JGE") -> "011",
    Some("JLT") -> "100",
    Some("JNE") -> "101",
    Some("JLE") -> "110",
    Some("JMP") -> "111"
  )
}
