package analyser

object CompilationEngine {
  def compileLet(tokeniser: Tokeniser): Either[String, LetStatement] =
    for {
      v <- getNextToken[VarName](tokeniser, VarName.from)
      _ <- assertNextTokenEquals(tokeniser, "=")
      e <- getNextToken[VarName](tokeniser, VarName.from)
    } yield LetStatement(v, e)

  //TODO constrain T to be a token
  private def getNextToken[T](tokeniser: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    tokeniser.advance()
    transformer(tokeniser.currentToken)

  private def assertNextTokenEquals(tokeniser: Tokeniser, equals: String): Either[String, Unit] =
    tokeniser.advance()
    tokeniser.currentToken match
      case string if string == equals => Right(())
      case _ => Left("uh-oh")
}
