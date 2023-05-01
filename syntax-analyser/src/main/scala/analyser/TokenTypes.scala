package analyser

object TokenTypes extends Enumeration {
  type TokenType = Value

  val Keyword, Symbol, Identifier, IntConst, StringConst = Value

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

  private val keywordsRegex =
    ("class|constructor|function|method|field|static|var|int|char|" +
      "boolean|void|true|false|null|this|let|do|if|else|while|return").r

  private val symbols = "{}()[].,;+-*/&|<>=~".split("").toList

  private val integerRegex = "^[1-9]\\d{0,4}$".r

  private val stringConstRegex = "^\"[^\"\\n]*\"$".r

  private val identifierRegex = "^[a-zA-Z_][a-zA-Z1-9_]*$".r
}
