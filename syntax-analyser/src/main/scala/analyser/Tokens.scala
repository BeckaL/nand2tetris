package analyser

import analyser.TokenTypes

trait Token

case class LetStatement(varname: Term, expression: Term) extends Token //TODO enable to handle expressions and arrays
case class DoStatement(identifier: Term, subroutineName: Term, arguments: List[Term]) extends Token

trait Term extends Token
case class VarName(value: String) extends Term
case class IntegerConstant(value: Integer) extends Term
case class StringConstant(s: String) extends Term

case class Expression(term: Term, opTerm: Option[(Operator, Term)]) extends Token
case class Operator(s: String) extends Token

object Term {
  def from(s: String): Either[String, Term] =
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(VarName(s))
      case TokenTypes.StringConst => Right(StringConstant(s.tail.dropRight(1)))
      case TokenTypes.IntConst => Right(IntegerConstant(s.toInt))
      case _ => Left(s"Uh-oh, tried to create an identifier from $s")
}

object Operator {
  def isOperator(s: String) = List("+", "-", "*", "/", "&", "|", "<", ">", "=").contains(s)
  def from(s: String): Either[String, Operator] = {
    if (isOperator(s))
      Right(Operator(s))
    else
      Left(s"Uh-oh, tried to created an operator from $s")
  }
}
