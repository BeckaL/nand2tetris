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
      case Add => binaryOperation(List("M=D+M"), "//add")
      case Sub => binaryOperation(List("M=D-M"), "//sub")
      case Gt => binaryOperation(jumpLogicForComparators("JGT"), "//gt")
      case Lt => binaryOperation(jumpLogicForComparators("JLT"), "//lt")
      case Eq => binaryOperation(jumpLogicForComparators("JEQ"), "//eq")
      case And => binaryOperation(List("M=M&D"), "//and")
      case Or => binaryOperation(List("M=D|M"), "//or")
      case Neg => unaryOperation(List("M=-M"), "//neg")
      case Not => unaryOperation(List("M=!M"), "//neg")
    }

  private def unaryOperation(setM: List[String], commentString: String): List[String] = {
    val getMOfTopStackValue = List(
      "@SP",
      "A=M-1",
    )
    //is this right and the one below?
    val setSPToCurrentAddressPlusOne = List(
      "D=A",
      "@SP",
      "M=D+1"
    )
    (commentString +: getMOfTopStackValue) ++ setM ++ setSPToCurrentAddressPlusOne
  }


  private def binaryOperation(setMTo: List[String], commentString: String): List[String] = {
    val setDAndMRegistersToTopTwoStackValues =  List(
      "@SP",
      "A=M-1",
      "D=M",
      "A=A-1",
    )
    val setSPToCurrentAddressPlusOne = List(
      "D=A",
      "@SP",
      "M=D+1"
    )
    (commentString +: setDAndMRegistersToTopTwoStackValues) ++ setMTo ++ setSPToCurrentAddressPlusOne
  }

  private def jumpLogicForComparators(jumpString: String) = List(
    "D=D-M",
    "@SET_M_TO_TRUE",
    s"D;$jumpString",
    "M=0",
    "(SET_M_TO_TRUE)",
    "M=1",
  )
}
