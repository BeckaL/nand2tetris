package parser

import parser.MemorySegment

object Translator {
  def translate(commands: List[Command], fileName: String): List[String] = {
    def translateTrackingNextN(commands: List[Command], asmLines: List[String], currentN: Int, returnN: Int): List[String] =
      commands match {
        case Nil => asmLines
        case firstCommand :: others => firstCommand match {
          case arithmenticCommand: ArithmeticAndLogicalCommand =>
            val (lines, nextN) = translateArithmeticAndLogicalCommand(arithmenticCommand, currentN)
            translateTrackingNextN(others, asmLines ++ lines, nextN, returnN)
          case memoryCommand: MemoryCommand =>
            val lines = memoryCommand match {
              case Push(memorySegment, i) => push(memorySegment, i, fileName)
              case Pop(memorySegment, i) => pop(memorySegment, i, fileName)
            }
            translateTrackingNextN(others, asmLines ++ lines, currentN, returnN)
          case functionCommand: FunctionCommand =>
            val (lines, newReturnN) = functionCommand match {
              case FunctionDeclaration(name, nArgs) => (functionDeclaration(name, nArgs), returnN)
              case FunctionReturn => (functionReturn, returnN)
              case FunctionCall(name, nArgs) => (functionCall(name, nArgs, returnN), returnN + 1)
            }
            translateTrackingNextN(others, asmLines ++ lines, currentN, newReturnN)
          case programFlowCommand: ProgramFlowCommand =>
            val lines = programFlowCommand match {
              case Label(name) => List(s"($name)")
              case GoTo(label) => List(s"@$label", "0;JMP")
              case IfGoTo(label) => storeTopStackValueInDAndDecrementSP ++ List(s"@$label", "D;JNE")
            }
            translateTrackingNextN(others, asmLines ++ lines, currentN, returnN)
        }
      }

    translateTrackingNextN(commands, List(), 0, 1) ++ end
  }

  private def functionCall(name: String, nArgs: Int, returnN: Int): List[String] = {
    def pushVar(variable: String): List[String] = List(s"//push $variable", s"@$variable", "D=M") ++ pushDToSpAndIncrement
    List(
      List(s"return.$returnN", "LCL", "ARG", "THIS", "THAT").flatMap(pushVar),
      List("//repositions arg", "@SP", "A=M") ++ List.fill(5 + nArgs)("A=A-1") ++ List("D=A", "@SP", "M=D"),
      List("//repositions LCL", "@SP", "D=M", "@LCL", "M=D"),
      List(s"//goTo function $name", s"@$name", "0;JMP"),
      List(s"(return.$returnN)")
    ).flatten
  }

  private def functionReturn: List[String] = {
    def setVarToPointerOfFrameMinusI(variable: String, minusI: Int) =
      List(s"//set $variable to frame minus $minusI", "@frame", "A=M") ++
        List.fill(minusI)("A=A-1") ++
        List("D=M", s"@$variable", "M=D")

    List(
      List("//set frame to LCL", "@LCL", "D=M", "@frame", "M=D"),
      setVarToPointerOfFrameMinusI("retAddr", 5),
      List("//reposition return value for caller") ++ storeTopStackValueInDAndDecrementSP ++ List("@ARG", "A=M", "M=D"),
      List("//repositionSpForCaller", "@ARG", "D=M", "D=D+1", "@SP", "M=D"),
      setVarToPointerOfFrameMinusI("THAT", 1),
      setVarToPointerOfFrameMinusI("THIS", 2),
      setVarToPointerOfFrameMinusI("ARG", 3),
      setVarToPointerOfFrameMinusI("LCL", 4),
      List("//goToRetAddr", "@retAddr", "A=M", "0;JMP")
    ).flatten
  }

  private def functionDeclaration(name: String, nArgs: Int): List[String] =
    s"($name)" +: (0 until nArgs).toList.flatMap(_ => push(CONSTANT, 0, "")) //filename doesn't matter here

  private val pushDToSpAndIncrement = List("@SP", "A=M", "M=D", "@SP", "M=M+1")

  private def push(memorySegment: MemorySegment, i: Int, fileName: String) = {
    val commentString = List(s"//push ${memorySegment.toString} $i")
    val storeValueFromMemorySegmentInD = memorySegment match {
      case STATIC => List(s"@$fileName.$i", "D=M")
      case CONSTANT => List(s"@$i", "D=A")
      case TEMP => List("@5", "D=A", s"@$i", "A=D+A", "D=M")
      case POINTER => List(pointerAddress(i), "D=M")
      case memSegPointer => List(s"@${memSegPointer.toString}", "D=M") ++ List(s"@$i", "A=D+A", "D=M")
    }
    commentString ++ storeValueFromMemorySegmentInD ++ pushDToSpAndIncrement
  }

  private def pop(memorySegment: MemorySegment, i: Int, fileName: String) = {
    val commentString = s"//pop ${memorySegment.toString} $i"
    val operation = memorySegment match {
      case CONSTANT => throw new RuntimeException(s"cannot pop to constant $i")
      case STATIC => storeTopStackValueInDAndDecrementSP ++ List(s"@$fileName.$i", "M=D")
      case POINTER => List(pointerAddress(i), "D=A", "@R13", "M=D") ++ popTopStackToAddressInR13
      case TEMP => List(s"@5", "D=A") ++ setR13ToDPlusI(i) ++ popTopStackToAddressInR13
      case memSegPointer => List(s"@${memSegPointer.toString}", "D=M") ++ setR13ToDPlusI(i) ++ popTopStackToAddressInR13
    }
    commentString +: operation
  }

  private def setR13ToDPlusI(i: Int) = List(s"@$i", "D=D+A", "@R13", "M=D")

  private val bootstrapCode = List("@256", "D=A", "@SP", "M=D") ++ functionCall("Sys.init", 0, 0)
  private val storeTopStackValueInDAndDecrementSP = List("@SP", "M=M-1", "A=M", "D=M")
  private val popTopStackToAddressInR13 = storeTopStackValueInDAndDecrementSP ++ List("@R13", "A=M", "M=D")

  private def pointerAddress(i: Int): String =
    i match {
      case 0 => "@3"
      case 1 => "@4"
      case _ => throw new RuntimeException(s"Don't know how to get pointer $i")
    }

  private def translateArithmeticAndLogicalCommand(command: ArithmeticAndLogicalCommand, nextN: Int): (List[String], Int) =
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

  private val end: List[String] = List(
    "(END)",
    "@END",
    "0;JMP"
  )
}
