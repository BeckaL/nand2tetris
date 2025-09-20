package analyser

import scala.util.chaining.scalaUtilChainingOps

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

  def compileClassVarDec(t: Tokeniser): MaybeLexicalElements = 
    for {
      staticOrField <- expectOneOfAndAdvance(t, List("static", "field"), s => Keyword(s))
      varType <- expectTypeAndAdvance(t)
      varNameLexElems <- parseNVars(t, List())
      _ <- expectStringAndAdvance(t, ";")
    } yield List(staticOrField, varType) ++ varNameLexElems :+ Symbol(';')


  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = ???

  def compileParameterList(t: Tokeniser): MaybeLexicalElements = ???

  def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements = ???
  
  def compileVarDec(t: Tokeniser): MaybeLexicalElements =
    for {
      varString <- expectStringAndAdvance(t, "var")
      varType <- expectTypeAndAdvance(t)
      varNameLexElems <- parseNVars(t, List())
      _ <- expectStringAndAdvance(t, ";")
    } yield List(Keyword("var"), varType) ++ varNameLexElems :+ Symbol(';')

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
      case TokenTypes.Identifier => safeAdvance(t).flatMap(_ => Right(Identifier(s)))
      case otherTokenType => Left(s"expected token type identifier for string $s but got $otherTokenType")

  private def expectStringAndAdvance(t: Tokeniser, toMatch: String): Either[String, Unit] =
    if (t.currentToken == toMatch)
      safeAdvance(t)
    else
      Left(s"expected $toMatch, got ${t.currentToken}")
      
  private def expectOneOfAndAdvance(t: Tokeniser, toMatch: List[String], transformer: String => LexicalElem): Either[String, LexicalElem] =
    val currentToken = t.currentToken
    if (toMatch.contains(currentToken)) 
      safeAdvance(t).flatMap(_ => Right(transformer(currentToken)))
    else
      Left(s"expected ${t.currentToken} toMatch one of $toMatch")

  private def expectTypeAndAdvance(t: Tokeniser): Either[String, LexicalElem] =
    val currentToken = t.currentToken
    TokenTypes.tokenType(currentToken) match {
      case TokenTypes.Keyword => 
        if (List("boolean", "char", "int").contains(currentToken))
         safeAdvance(t).flatMap(_ => Right(Keyword(currentToken)))
        else 
          Left(s"keyword $currentToken cannot be used as a type, valid keyword types are boolean char or int")
      case TokenTypes.Identifier => 
        Right(Identifier(currentToken)).tap(_ => t.advance())
      case other =>
        Left(s"$currentToken cannot be used as a type")
    }

  private def stringConstFromQuotedString(s: String): StringConst =
    StringConst(s.tail.dropRight(1))


  private def parseNVars(t: Tokeniser, soFar: List[LexicalElem]): MaybeLexicalElements =
    expectVar(t).flatMap(varNameLexElem =>
      t.currentToken match {
        case ";" => Right(soFar :+ varNameLexElem)
        case "," => safeAdvance(t).flatMap(_ =>
          parseNVars(t, soFar ++ List(varNameLexElem, Symbol(',')))
        )
        case otherToken => Left(s"expected either ';' or ',' when parsing vars, got $otherToken")
      })

  private def safeAdvance(t: Tokeniser): Either[String, Unit] =
    if (t.hasMoreTokens)
      t.advance()
      Right(())
    else Left("unexpected end of input")
}
