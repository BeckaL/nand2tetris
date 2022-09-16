package parser

sealed trait Command

sealed trait ArithmeticAndLogicalCommand extends Command
case object Add extends ArithmeticAndLogicalCommand
case object Sub extends ArithmeticAndLogicalCommand
case object Eq extends ArithmeticAndLogicalCommand
case object Gt extends ArithmeticAndLogicalCommand
case object Lt extends ArithmeticAndLogicalCommand
case object And extends ArithmeticAndLogicalCommand
case object Or extends ArithmeticAndLogicalCommand
case object Not extends ArithmeticAndLogicalCommand
case object Neg extends ArithmeticAndLogicalCommand

sealed trait MemoryCommand extends Command
case class Pop(memorySegment: MemorySegment, i: Int) extends MemoryCommand
case class Push(memorySegment: MemorySegment, i: Int) extends MemoryCommand
