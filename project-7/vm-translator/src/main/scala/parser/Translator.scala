package parser

object Translator {
  def translate(commands: List[Command]): List[String] = {
    def translateTrackingNextN(commands: List[Command], asmLines: List[String], currentN: Int): List[String] =
      commands match {
        case Nil => asmLines
        case firstCommand :: others => firstCommand match {
          case arithmenticCommand: ArithmeticAndLogicalCommand =>
            val (lines, nextN) = translateArithmeticAndLogicalCommand(arithmenticCommand, currentN)
            translateTrackingNextN(others, asmLines ++ lines, nextN)
          case memoryCommand: MemoryCommand =>
            val lines = translateMemoryCommand(memoryCommand)
            translateTrackingNextN(others, asmLines ++ lines, currentN)
        }
      }
    translateTrackingNextN(commands, List(), 0) ++ end
  }

  def translateMemoryCommand(command: MemoryCommand): List[String] =
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

  def translateArithmeticAndLogicalCommand(command: ArithmeticAndLogicalCommand, nextN: Int): (List[String], Int) =
    command match {
      case Add => (binaryOperation(List("M=D+M"), "//add"), nextN)
      case Sub => (binaryOperation(List("M=M-D"), "//sub"), nextN)
      case And => (binaryOperation(List("M=M&D"), "//and"),  nextN)
      case Or => (binaryOperation(List("M=D|M"), "//or"), nextN)
      case Gt => (binaryComparatorOperation("JGT", nextN, "//gt"), nextN + 1)
      case Lt => (binaryComparatorOperation("JLT", nextN, "//lt"), nextN + 1)
      case Eq => (binaryComparatorOperation("JEQ", nextN, "//eq"), nextN + 1)
      case Neg => (unaryOperation(List("M=-M"), "//neg"), nextN)
      case Not => (unaryOperation(List("M=!M"), "//not"), nextN)
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

  private val setDAndMRegistersToTopTwoStackValues =  List(
    "@SP",
    "A=M-1",
    "D=M",
    "A=A-1",
  )

  private def binaryComparatorOperation(comparatorString: String, nextN: Int, commentString: String) =
    (commentString +: setDAndMRegistersToTopTwoStackValues) ++ jumpLogicForComparators(comparatorString, nextN)


  private def binaryOperation(setMTo: List[String], commentString: String): List[String] = {
    val setSPToCurrentAddressPlusOne = List(
      "D=A",
      "@SP",
      "M=D+1"
    )
    (commentString +: setDAndMRegistersToTopTwoStackValues) ++ setMTo ++ setSPToCurrentAddressPlusOne
  }

  private val end: List[String] = List(
    "(END)",
    "@END",
    "0;JMP"
  )

  private def jumpLogicForComparators(jumpString: String, n: Int) = List(
    "D=M-D",
    "@SP",
    "M=M-1",
    "M=M-1",
    "A=M",
    "M=0", //set it to false first, and jump to true only if condition holds
    s"@SET_M_TO_TRUE.$n",
    s"D;$jumpString",
    s"@AFTER_LOGICAL_COMPARISON.$n",
    "0;JMP", // jump to end if we haven't jumped to setting to true by this ponint
    s"(SET_M_TO_TRUE.$n)",
    "@SP",
    "A=M",
    "M=-1",
    s"(AFTER_LOGICAL_COMPARISON.$n)", // continue to next instructions after this bit of logic
    "@SP",
    "M=M+1"
  )
}
