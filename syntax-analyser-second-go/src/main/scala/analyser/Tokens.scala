package analyser

trait LexicalElement {
  def toTokenString: String
}

case class LexicalSymbol(s: Char) extends LexicalElement {
  override def toTokenString = s"<symbol> ${scala.xml.Utility.escape(s.toString)} </symbol>"
}

case class Keyword(string: String) extends LexicalElement {
  override def toTokenString = s"<keyword> $string </keyword>"
}

case class LexicalIntegerConstant(i: Int) extends LexicalElement {
  override def toTokenString: String = s"<integerConstant> $i </integerConstant>"
}

case class LexicalStringConstant(s: String) extends LexicalElement {
  override def toTokenString: String = s"<stringConstant> $s </stringConstant>"
}

case class LexicalIdentifier(id: String) extends LexicalElement {
  override def toTokenString: String = s"<identifier> $id </identifier>"
}

case class UnaryOperator(s: Char) extends LexicalElement {
  override def toTokenString: String = s"<symbol> $s </symbol>"
}

object LexicalElement {
  def keywordOrIndentifierFrom(s: String): Either[String, LexicalElement] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword => Right(Keyword(s))
      case TokenTypes.Identifier => Right(LexicalIdentifier(s))
      case _ => Left(s"Uh-oh, tried to parse keyword or identifier from $s")
  }

  def intConstant(s: String): LexicalIntegerConstant = LexicalIntegerConstant(s.toInt)

  def strConstant(s: String): LexicalStringConstant = LexicalStringConstant(s.tail.dropRight(1))

  def varDecTypeFrom(s: String, classVarDec: Boolean): Either[String, Keyword] = {
    val allowedTypes = if (classVarDec) TokenTypes.CLASS_VAR_TYPES else Set("var")
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword if allowedTypes.contains(s) => Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a var dec type")
      case _ => Left(s"Uh-oh, tried to parse keyword or identifier from $s")
  }

  def subroutineTypeFrom(s: String): Either[String, Keyword] = {
    TokenTypes.tokenType(s) match {
      case TokenTypes.Keyword if TokenTypes.SUBROUTINE_TYPES.contains(s) =>
        Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a subroutine type")
      case _ => Left(s"Uh-oh, tried to parse subroutine type from $s")
    }
  }

  def returnTypeFrom(s: String): Either[String, LexicalElement] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword if TokenTypes.SUBROUTINE_RETURN_TYPES.contains(s) =>
        Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a return type")
      case TokenTypes.Identifier => Right(LexicalIdentifier(s))
      case _ => Left(s"Uh-oh, tried to parse return type or identifier from $s")
  }

  def keywordFrom(s: String): Either[String, Keyword] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword => Right(Keyword(s))
      case _ => Left(s"Uh-oh, tried to parse keyword from $s")
  }

  def identifierFrom(s: String): Either[String, LexicalIdentifier] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(LexicalIdentifier(s))
      case _ => Left(s"Uh-oh, tried to parse identifier from $s")
  }

  def operatorFrom(s: String, allowedTypes: Set[String]): Either[String, LexicalSymbol] = {
    if (allowedTypes.contains(s))
      Right(LexicalSymbol(s.head))
    else
      Left(s"Uh-oh, tried to construct operator from $s, allowed types were $allowedTypes")
  }
}

trait Token

case class LetStatement(varname: Term, expression: Term) extends Token //TODO enable to handle expressions and arrays

case class DoStatement(identifier: Term, subroutineName: Term, arguments: List[Term]) extends Token

trait Term extends Token {
  def toLexElem: LexicalElement
}

case class VarName(value: String) extends Term {
  override def toLexElem: LexicalElement = LexicalIdentifier(value)
}

case class IntegerConstant(value: Integer) extends Term {
  override def toLexElem: LexicalElement = LexicalIntegerConstant(value)
}

case class StringConstant(s: String) extends Term {
  override def toLexElem: LexicalElement = LexicalStringConstant(s)
}

case class Expression(term: Term, opTerm: Option[(Operator, Term)]) extends Token

case class Operator(s: String) extends Token {
  def toLexElem: LexicalSymbol = LexicalSymbol(s.head)
}

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
