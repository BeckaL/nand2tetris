package analyser

trait LexicalElem {
  def toTokenString: String
}

case class Symbol(s: Char) extends LexicalElem {
  override def toTokenString = s"<symbol> ${scala.xml.Utility.escape(s.toString)} </symbol>"
}

case class Keyword(string: String) extends LexicalElem {
  override def toTokenString = s"<keyword> $string </keyword>"
}

case class IntConst(i: Int) extends LexicalElem {
  override def toTokenString: String = s"<integerConstant> $i </integerConstant>"
}

case class StringConst(s: String) extends LexicalElem {
  override def toTokenString: String = s"<stringConstant> $s </stringConstant>"
}

case class Identifier(id: String) extends LexicalElem {
  override def toTokenString: String = s"<identifier> $id </identifier>"
}

case class UnaryOperator(s: Char) extends LexicalElem {
  override def toTokenString: String = s"<symbol> $s </symbol>"
}

object LexicalElem {
  def keywordOrIndentifierFrom(s: String): Either[String, LexicalElem] = 
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword => Right(Keyword(s))
      case TokenTypes.Identifier => Right(Identifier(s))
      case _ => Left(s"Uh-oh, tried to parse keyword or identifier from $s")

  def intConstant(s: String): IntConst = IntConst(s.toInt)

  def strConstant(s: String): StringConst = StringConst(s.tail.dropRight(1))

  def varDecTypeFrom(s: String, classVarDec: Boolean): Either[String, Keyword] = 
    val allowedTypes = if (classVarDec) TokenTypes.CLASS_VAR_TYPES else Set("var")
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword if allowedTypes.contains(s) => Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a var dec type")
      case _ => Left(s"Uh-oh, tried to parse keyword or identifier from $s")

  def subroutineTypeFrom(s: String): Either[String, Keyword] = 
    TokenTypes.tokenType(s) match {
      case TokenTypes.Keyword if TokenTypes.SUBROUTINE_TYPES.contains(s) =>
        Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a subroutine type")
      case _ => Left(s"Uh-oh, tried to parse subroutine type from $s")
    }

  def returnTypeFrom(s: String): Either[String, LexicalElem] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword if TokenTypes.SUBROUTINE_RETURN_TYPES.contains(s) =>
        Right(Keyword(s))
      case TokenTypes.Keyword => Left(s"keyword $s cannot be used as a return type")
      case TokenTypes.Identifier => Right(Identifier(s))
      case _ => Left(s"Uh-oh, tried to parse return type or identifier from $s")
  }

  def keywordFrom(s: String): Either[String, Keyword] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Keyword => Right(Keyword(s))
      case _ => Left(s"Uh-oh, tried to parse keyword from $s")
  }

  def identifierFrom(s: String): Either[String, Identifier] = {
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(Identifier(s))
      case _ => Left(s"Uh-oh, tried to parse identifier from $s")
  }

  def operatorFrom(s: String, allowedTypes: Set[String]): Either[String, Symbol] = {
    if (allowedTypes.contains(s))
      Right(Symbol(s.head))
    else
      Left(s"Uh-oh, tried to construct operator from $s, allowed types were $allowedTypes")
  }
}
