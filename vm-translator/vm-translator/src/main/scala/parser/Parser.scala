package parser
import utils.implicits._

object Parser {
  val PopPattern = "pop ([A-Za-z][\\S]*) ([0-9]+)".r
  val PushPattern = "push ([A-Za-z][\\S]*) ([0-9]+)".r
  val FunctionDeclarationPattern = "function ([A-Za-z][\\S]*) ([0-9]+)".r
  val LabelPattern = "label ([A-Za-z][\\S]*)".r
  val GoToPattern = "goto ([A-Za-z][\\S]*)".r
  val IfGoToPattern = "if-goto ([A-Za-z][\\S]*)".r

  def parse(instructions: List[String]): List[Command] =
    instructions.map(_.stripCommentsAndWhitespace).filterNot(_.isEmpty).map(parseLine)

  def parseLine(str: String): Command = str match {
    case "add" =>  Add
    case "sub" =>  Sub
    case "neg" =>  Neg
    case "eq" =>  Eq
    case "gt" =>  Gt
    case "lt" =>  Lt
    case "and" =>  And
    case "or" =>  Or
    case "not" =>  Not
    case "return" => FunctionReturn
    case PopPattern(memoryString, intString) => Pop(parseMemorySegment(memoryString), intString.toInt)
    case PushPattern(memoryString, intString) => Push(parseMemorySegment(memoryString), intString.toInt)
    case FunctionDeclarationPattern(functionName, intString) => FunctionDeclaration(functionName, intString.toInt)
    case LabelPattern(labelName) => Label(labelName)
    case GoToPattern(labelName) => GoTo(labelName)
    case IfGoToPattern(labelName) => IfGoTo(labelName)
    case otherString => throw new RuntimeException(s"couldn't parse string $otherString into command")  //we can assume correct inputs for now
  }

  def parseMemorySegment(string: String) =
    string match {
      case "local" => LCL
      case "argument" => ARG
      case "this" => THIS
      case "that" => THAT
      case "constant" => CONSTANT
      case "temp" => TEMP
      case "static" => STATIC
      case "pointer" => POINTER
      case otherString => throw new RuntimeException(s"couldn't parse string $otherString into memory segment")  //we can assume correct inputs for now
    }
}
