package parser

object Translator {
  def translate(commands: List[Command]): List[String] =
    commands.flatMap {
      case arithmenticCommand: ArithmeticAndLogicalCommand => translateArithmeticAndLogicalCommand(arithmenticCommand)
      case memoryCommand: MemoryCommand => translateMemoryCommend(memoryCommand)
    }

  def translateMemoryCommend(command: MemoryCommand): List[String] =
    command match {
      case Push(CONSTANT, i) =>
        List(
          s"//push CONSTANT $i",
          s"@$i",
          "D=A",
          "@SP",
          "A=M",
          "M=D",
          "@SP",
          "M=M+1"
        )
      case _ => ???
    }

  def translateArithmeticAndLogicalCommand(command: ArithmeticAndLogicalCommand): List[String] =
    command match {
      case Add => ("//add" +: setDAndMRegistersToTopTwoStackValues) ++ ("M=D+M" +: setSPToCurrentAddressPlusOne)
      case Sub => ("//sub" +: setDAndMRegistersToTopTwoStackValues) ++ ("M=D-M" +: setSPToCurrentAddressPlusOne)
      case _ => ???
    }

  private val setSPToCurrentAddressPlusOne = List(
    "D=A",
    "@SP",
    "M=D+1"
  )

  private val setDAndMRegistersToTopTwoStackValues =  List(
    "@SP",
    "A=M-1",
    "D=M",
    "A=A-1",
  )
}
