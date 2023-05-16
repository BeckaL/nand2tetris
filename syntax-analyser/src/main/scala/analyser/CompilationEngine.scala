package analyser

object CompilationEngine {
  def compileLet(tokeniser: Tokeniser): Either[String, LetStatement] =
    for {
      variable <- getNextToken[VarName](tokeniser, VarName.from)
      _ <- assertNextTokenEquals(tokeniser, "=")
      expression <- getNextToken[VarName](tokeniser, VarName.from)
      _ <- assertNextTokenEquals(tokeniser, ";")
    } yield LetStatement(variable, expression)

  def compileDo(tokeniser: Tokeniser): Either[String, DoStatement] =
    for {
      variable <- getNextToken[VarName](tokeniser, VarName.from)
      _ <- assertNextTokenEquals(tokeniser, ".")
      method <- getNextToken[VarName](tokeniser, VarName.from)
      _ <- assertNextTokenEquals(tokeniser, "(")
      list <- getOptionalListOfVarsFollowedByClosingChar(tokeniser)
      _ <- assertNextTokenEquals(tokeniser, ";")
    } yield DoStatement(variable, method, list)

  //TODO tidy this up
  private def getOptionalListOfVarsFollowedByClosingChar(tokeniser: Tokeniser, varListSoFar: List[VarName] = List()): Either[String, List[VarName]] =
    tokeniser.advance()
    tokeniser.currentToken match {
      case ")" => Right(varListSoFar)
      case _ => getVarList(tokeniser, List())
    }

  private def getVarList(tokeniser: Tokeniser, soFar: List[VarName] = List()): Either[String, List[VarName]] = {
    (for {
      varName <- VarName.from(tokeniser.currentToken)
      nextToken <- assertNextTokenEqualsOneOf(tokeniser, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar :+ varName
    } yield (newVarList, continue)) match {
      case Right((newVarList, true)) =>
        tokeniser.advance()
        getVarList(tokeniser, newVarList)
      case Right((newVarList, false)) => Right(newVarList)
      case Left(e) => Left(e)
    }
  }

  private def assertNextTokenEqualsOneOf(tokeniser: Tokeniser, equals: Set[String]): Either[String, String] =
    tokeniser.advance()
    tokeniser.currentToken match
      case string if equals.contains(string) => Right(string)
      case otherString => Left(s"uh-oh, expected $otherString to equal $equals")

  //TODO constrain T to be a token
  private def getNextToken[T](tokeniser: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    tokeniser.advance()
    transformer(tokeniser.currentToken)

  private def assertNextTokenEquals(tokeniser: Tokeniser, equals: String): Either[String, Unit] =
    tokeniser.advance()
    tokeniser.currentToken match
      case string if string == equals => Right(())
      case otherString => Left(s"uh-oh, expected $otherString to equal $equals")
}
