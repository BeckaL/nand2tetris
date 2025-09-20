package analyser

import util.chaining.scalaUtilChainingOps

object CompilationEngine {
  private type MaybeLexicalElements = Either[String, List[LexicalElem]]

  def compileLet(t: Tokeniser): MaybeLexicalElements = {
    for {
      _ <- expectStringAndAdvance(t, "let")
      varName <- expectVar(t)
      _ <- expectStringAndAdvance(t, "=")
      term <- expectTerm(t)
      _ <- expectStringAndAdvance(t, ";")
    } yield List(Keyword("let"), varName, Symbol('='), term, Symbol(';'))
  }

  def compileClass(t: Tokeniser): MaybeLexicalElements = ???

  def compileClassVarDec(t: Tokeniser): MaybeLexicalElements = ???

  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = ???

  def compileParameterList(t: Tokeniser): MaybeLexicalElements = ???

  def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements = ???
  def compileVarDec(t: Tokeniser): MaybeLexicalElements = ???
  def compileStatements(t: Tokeniser): MaybeLexicalElements = ???
  def compileIf(t: Tokeniser): MaybeLexicalElements = ???
  def compileWhile(t: Tokeniser): MaybeLexicalElements = ???
  def compileDo(t: Tokeniser): MaybeLexicalElements = ???
  def compileReturn(t: Tokeniser): MaybeLexicalElements = ???
  def compileExpression(t: Tokeniser): MaybeLexicalElements = ???
  def compileExpressionList(t: Tokeniser): Either[String, (List[LexicalElem], Int)] = ???
  def compileTerm(t: Tokeniser): MaybeLexicalElements = ???

  private def expectTerm(t: Tokeniser): Either[String, LexicalElem] =
    val s = t.currentToken
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(Identifier(s)).tap(_ => t.advance())
      case TokenTypes.IntConst => Right(IntConst(s.toInt)).tap(_ => t.advance())
      case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Right(Keyword(s)).tap(_ => t.advance())
      case TokenTypes.StringConst => Right(stringConstFromQuotedString(s)).tap(_ => t.advance())
      case otherTokenType => Left(s"expected valid term to be found in string $s but got $otherTokenType")

  private def expectVar(t: Tokeniser): Either[String, LexicalElem] =
    val s = t.currentToken
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(Identifier(s)).tap(_ => t.advance())
      case otherTokenType => Left(s"expected token type identifier for string $s but got $otherTokenType")

  private def expectStringAndAdvance(t: Tokeniser, toMatch: String): Either[String, Unit] =
    if (t.currentToken == toMatch)
      Right(()).tap(_ => t.advance())
      //when to call has more tokens?
    else
      Left(s"expected let, got ${t.currentToken}")

  private def stringConstFromQuotedString(s: String): StringConst =
    StringConst(s.tail.dropRight(1))
}
