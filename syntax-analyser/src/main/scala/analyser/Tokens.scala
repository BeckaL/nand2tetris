package analyser

import analyser.TokenTypes

trait Token

case class LetStatement(varname: VarName, expression: VarName) extends Token //TODO enable to handle expressions and arrays
case class VarName(value: String) extends Token

object VarName {
  def from(s: String): Either[String, VarName] =
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(VarName(s))
      case _ => Left("Uh-oh")
}
