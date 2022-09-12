package assembler

object Constants {
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
