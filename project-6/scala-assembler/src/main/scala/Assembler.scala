object Assembler {
  def parseAsBinaryString(instructions: List[String]): List[String] =
    instructions.map(str =>
      str.trim.toList match {
        case '@' :: rest => Some(aInstruction(rest.mkString("")))
        case '/' :: '/' :: _ => None
        case Nil => None
        case cInstructionString => Some(cInstruction(cInstructionString.mkString("")))
      }
    ).collect{ case Some(binString) => binString}

  private def aInstruction(string: String): String = {
    val trimmedString = string.stripCommentsAndWhitespace
    PREDEFINED_SYMBOLS.get(trimmedString) match {
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
    "111" + compBits + destBits + jumpBits(maybeJump)
  }

  private def computationBits(string: String): String =
    string.stripCommentsAndWhitespace match {
      case mString if string.contains("M") => "1" + COMPUTATION_BITS_MAP(mString.replace("M", "A"))
      case aString => "0" + COMPUTATION_BITS_MAP(aString)
    }

  private def destinationBits(string: String): String =
    List(string.contains("A"), string.contains("D"), string.contains("M")).map{ b => if (b) '1' else '0' }.mkString("")

  private def jumpBits(jumpString: Option[String]): String =
    JUMP_BITS_MAP(jumpString.map(_.stripCommentsAndWhitespace))

  implicit class StringOps(string: String) {
    def splitIntoMaxTwo(splitString: String): (String, Option[String]) =
      string.indexOf(splitString) match {
        case -1 => (string.trim, None)
        case n =>
          val (first, second) = string.splitAt(n)
          (first.trim, Some(second.drop(1).trim))
      }

    def toPaddedBinaryString(requiredSize: Int): String =
      string.toInt.toPaddedBinaryString(requiredSize)

    def stripCommentsAndWhitespace: String = splitIntoMaxTwo("//")._1
  }

  implicit class IntOps(i: Int) {
    def toPaddedBinaryString(requiredSize: Int) = {
      val unpaddedBinary = i.toBinaryString
      "0" * (requiredSize - unpaddedBinary.length) + unpaddedBinary
    }
  }

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
