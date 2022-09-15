package parser

object Translator {
  def translate(commands: List[Command], fileName: String): List[String] = {
    def translateTrackingNextN(commands: List[Command], asmLines: List[String], currentN: Int): List[String] =
      commands match {
        case Nil => asmLines
        case firstCommand :: others => firstCommand match {
          case arithmenticCommand: ArithmeticAndLogicalCommand =>
            val (lines, nextN) = translateArithmeticAndLogicalCommand(arithmenticCommand, currentN)
            translateTrackingNextN(others, asmLines ++ lines, nextN)
          case memoryCommand: MemoryCommand =>
            val lines = translateMemoryCommand(memoryCommand, fileName)
            translateTrackingNextN(others, asmLines ++ lines, currentN)
        }
      }

    translateTrackingNextN(commands, List(), 0) ++ end
  }

  private val pushDToSpAndIncrement = List("@SP", "A=M", "M=D", "@SP", "M=M+1")
  private val storeTopStackValueInDAndDecrementSP = List("@SP", "M=M-1", "A=M", "D=M")

  def translateMemoryCommand(command: MemoryCommand, fileName: String): List[String] = {
    command match {
      case Push(CONSTANT, i) =>
        List(
          s"//push CONSTANT $i",
          s"@$i",
          "D=A"
        ) ++ pushDToSpAndIncrement
      case Push(STATIC, i) =>
        List(
          s"//push STATIC $i",
          s"@$fileName.$i",
          "D=M",
        ) ++ pushDToSpAndIncrement
      case Pop(STATIC, i) =>
        (s"//pop STATIC $i" +: storeTopStackValueInDAndDecrementSP) ++
          List(
            s"@$fileName.$i",
            "M=D"
          )
      case Pop(ARG, i) => popToPointer("ARG", i)
      case Pop(THIS, i) => popToPointer("THIS", i)
      case Pop(THAT, i) => popToPointer("THAT", i)
      case Pop(LCL, i) => popToPointer("LCL", i)
      case Pop(TEMP, i) => popToTemp(i)
      case Push(ARG, i) => pushFromPointer("ARG", i)
      case Push(THIS, i) => pushFromPointer("THIS", i)
      case Push(THAT, i) => pushFromPointer("THAT", i)
      case Push(LCL, i) => pushFromPointer("LCL", i)
      case Push(TEMP, i) => pushFromTemp(i)
      case c => throw new RuntimeException(s"Don't know how to implement ${c}")
    }
  }

  private def pushFromTemp(i: Int) =
    List(
      s"//push TEMP $i",
      "@5",
      "D=A") ++ pushFromDPlusI(i)

  private def pushFromDPlusI(i: Int) =
    List(
      s"@$i",
      "A=D+A",
      "D=M", //store get value from arg i in D
    ) ++ pushDToSpAndIncrement

  private def pushFromPointer(pointerString: String, i: Int): List[String] =
    List(
      s"//push $pointerString $i",
      s"@$pointerString",
      "D=M"
    ) ++ pushFromDPlusI(i)


  private def popToTemp(i: Int) = {
    List(
      s"//pop TEMP $i",
      s"@5",
      "D=A",
      s"@$i",
      "D=D+A",
      "@R13",
      "M=D", //store address to pop to in R13
    ) ++ popTopStackToAddressInR13
  }

  private def popTopStackToAddressInR13 =
    storeTopStackValueInDAndDecrementSP ++ List("@R13", "A=M", "M=D")

  private def popToPointer(pointerString: String, i: Int): List[String] =
    List(
      s"//pop $pointerString $i",
      s"@${pointerString}",
      "D=M",
      s"@$i",
      "D=D+A",
      "@R13",
      "M=D", //store address to pop to in R13
    ) ++ popTopStackToAddressInR13

  def translateArithmeticAndLogicalCommand(command: ArithmeticAndLogicalCommand, nextN: Int): (List[String], Int) =
    command match {
      case Add => (binaryOperation(List("M=D+M"), "//add"), nextN)
      case Sub => (binaryOperation(List("M=M-D"), "//sub"), nextN)
      case And => (binaryOperation(List("M=M&D"), "//and"), nextN)
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

  private val setDAndMRegistersToTopTwoStackValues = List(
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
