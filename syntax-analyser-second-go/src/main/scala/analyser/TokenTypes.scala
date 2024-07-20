package analyser

object TokenTypes extends Enumeration {
  type TokenType = Value

  val Keyword, Symbol, Identifier, IntConst, StringConst = Value

  val SUBROUTINE_RETURN_TYPES: Set[String] = Set("void", "int", "char", "boolean")
  val SUBROUTINE_TYPES: Set[String] = Set("constructor", "function", "method")
  val CLASS_VAR_TYPES: Set[String] = Set("static", "field")
  val KEYWORD_CONSTANTS: Set[String] = Set("true", "false", "null", "this")
  val UNARY_OPERATORS = Set("-", "~")

  def tokenType(currentToken: String): TokenType =
    if (keywordsRegex.matches(currentToken))
      Keyword
    else if (symbols.contains(currentToken))
      Symbol
    else if (integerRegex.matches(currentToken) && currentToken.toInt <= 32767)
      IntConst
    else if (stringConstRegex.matches(currentToken))
      StringConst
    else if (identifierRegex.matches(currentToken))
      Identifier
    else
      throw new RuntimeException(s"Couldn't parse token: $currentToken is not a valid token")

  val keywordsRegex =
    ("class|constructor|function|method|field|static|var|int|char|" +
      "boolean|void|true|false|null|this|let|do|if|else|while|return").r

  val symbols = "{}()[].,;+-*/&|<>=~".split("").toList
  
  val integerRegex = "^[1-9]\\d{0,4}|0$".r

  val stringConstRegex = "^\"[^\"\\n]*\"$".r

  val identifierRegex = "^[a-zA-Z_][a-zA-Z1-9_]*$".r
}
