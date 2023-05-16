package analyser

import util.chaining.scalaUtilChainingOps

object CompilationEngine {
  def compileLet(tokeniser: Tokeniser): Either[String, LetStatement] =
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      variable <- getTokenAsAndAdvance[VarName](tokeniser, VarName.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      expression <- getTokenAsAndAdvance[VarName](tokeniser, VarName.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
    } yield LetStatement(variable, expression)

  def compileDo(t: Tokeniser): Either[String, DoStatement] =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "do")
      variable <- getTokenAsAndAdvance[VarName](t, VarName.from)
      _ <- assertTokenEqualsAndAdvance(t, ".")
      method <- getTokenAsAndAdvance[VarName](t, VarName.from)
      _ <- assertTokenEqualsAndAdvance(t, "(")
      list <- getOptionalListOfVarsFollowedByClosingChar(t)
      _ <- assertTokenEqualsAndAdvance(t, ";")
    } yield DoStatement(variable, method, list)

  private def getOptionalListOfVarsFollowedByClosingChar(t: Tokeniser, varListSoFar: List[VarName] = List()): Either[String, List[VarName]] =
    t.currentToken match
      case ")" => Right(varListSoFar).tap(_ => t.advance())
      case _ => getVarList(t, List())

  private def getVarList(t: Tokeniser, soFar: List[VarName] = List()): Either[String, List[VarName]] =
    for {
      varName <- getTokenAsAndAdvance[VarName](t, VarName.from)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar :+ varName
      r <- if (continue)
          getVarList(t, newVarList)
        else
          Right(newVarList)
    } yield r

  private def assertNextTokenEqualsOneOf(t: Tokeniser, equals: Set[String]): Either[String, String] =
    t.currentToken match
      case s if equals.contains(s) => Right(s).tap(_ => t.advance())
      case otherString => Left(s"uh-oh, expected $otherString to equal ${equals.toList.mkString(" or ")}")

  private def getTokenAsAndAdvance[T <: Token](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def assertTokenEqualsAndAdvance(t: Tokeniser, equals: String): Either[String, Unit] =
    assertNextTokenEqualsOneOf(t, Set(equals)).map(_ => ())
}
