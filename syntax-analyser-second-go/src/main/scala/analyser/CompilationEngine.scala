package analyser

import scala.annotation.tailrec
import util.chaining.scalaUtilChainingOps

object CompilationEngine {

  private type MaybeLexicalElements = Either[String, List[LexicalElem]]

  def compileLet(tokeniser: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      variable <- getLexElementAsAndAdvance[Identifier](tokeniser, LexicalElem.identifierFrom)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      expression <- compileExpression(tokeniser)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
    } yield List(
      Keyword("let"), variable, Symbol('=')) ++ expression :+ Symbol(';')

  def compileDo(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "do")
      variable <- getLexElementAsAndAdvance[Identifier](t, LexicalElem.identifierFrom)
      _ <- assertTokenEqualsAndAdvance(t, ".")
      method <- getLexElementAsAndAdvance[Identifier](t, LexicalElem.identifierFrom)
      _ <- assertTokenEqualsAndAdvance(t, "(")
      list <- getOptionalListOfVarsFollowedByClosingChar(t)
      _ <- assertTokenEqualsAndAdvance(t, ";")
    } yield List(Keyword("do"), variable, Symbol('.'), method) ++ wrapBrackets(list) :+ Symbol(';')

  private def wrapBrackets(l: List[LexicalElem]): List[LexicalElem] = (Symbol('(') +: l) :+ Symbol(')')

  private def wrapCurlyBrackets(l: List[LexicalElem]): List[LexicalElem] = (Symbol('{') +: l) :+ Symbol('}')

  def compileVarDec(t: Tokeniser, classDec: Boolean = false): MaybeLexicalElements =
    for {
      decType <- getLexElementAsAndAdvance[Keyword](t, LexicalElem.varDecTypeFrom(_, classDec))
      varType <- getLexElementAsAndAdvance[LexicalElem](t, LexicalElem.keywordOrIndentifierFrom)
      varNames <- getVarDecList(t)
    } yield List(decType, varType) ++ varNames :+ Symbol(';')

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "return")
      //TODO handle non empty returns
      _ <- assertTokenEqualsAndAdvance(t, ";")
    } yield List(Keyword("return"), Symbol(';'))

  //TODO make compileStatements
  def compileStatement(t: Tokeniser): MaybeLexicalElements =
    t.currentToken match
      case "let" => compileLet(t)
      case "do" => compileDo(t)
      case "if" => compileIf(t)
      case "return" => compileReturn(t)
      case otherToken => Left(s"uh oh $otherToken")

  def compileParameterList(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "(")
      paramList <- getOptionalVarParamList(t)
    } yield Symbol('(') +: paramList :+ Symbol(')')

  def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "{")
      varDecs <- compileOptionalVarDecs(t)
      statements <- compileOptionalStatements(t)
      _ <- assertTokenEqualsAndAdvance(t, "}")
    } yield Symbol('{') +: List(varDecs, statements).flatten :+ Symbol('}')

  def compileSubroutine(t: Tokeniser): MaybeLexicalElements =
    for {
      subroutineType <- getLexElementAsAndAdvance[Keyword](t, LexicalElem.subroutineTypeFrom)
      returnType <- getLexElementAsAndAdvance[LexicalElem](t, LexicalElem.returnTypeFrom)
      subroutineName <- getLexElementAsAndAdvance[LexicalElem](t, LexicalElem.identifierFrom)
      params <- compileParameterList(t)
      body <- compileSubroutineBody(t)
    } yield List(subroutineType, returnType, subroutineName) ++ params ++ body

  def compileClass(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "class")
      className <- getLexElementAsAndAdvance[Identifier](t, LexicalElem.identifierFrom)
      _ <- assertTokenEqualsAndAdvance(t, "{")
      classVars <- compileOptionalVarDecs(t, true)
      subroutines <- compileOptionalSubroutines(t)
      _ <- assertTokenEqualsAndAdvance(t, "}")
    } yield List(Keyword("class"), className, Symbol('{')) ++ classVars ++ subroutines ++ List(Symbol('}'))

  def compileTerm(t: Tokeniser): MaybeLexicalElements =
    val s = t.currentToken
    val lexElemOrError = TokenTypes.tokenType(s) match
      case TokenTypes.IntConst => LexicalElem.intConstant(s)
      case TokenTypes.StringConst => LexicalElem.strConstant(s)
      case TokenTypes.Identifier => Identifier(s)
      case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Keyword(s)
      case TokenTypes.Keyword => s"Cannot create keyword const from keyword $s"
      case TokenTypes.Symbol if s == "(" =>
        t.advance()
        for {
          e <- compileExpression(t)
          _ <- assertTokenEqualsAndAdvance(t, ")")
        } yield wrapBrackets(e)
      case TokenTypes.Symbol if TokenTypes.UNARY_OPERATORS.contains(s) =>
        for {
          unaryOp <- getLexElementAsAndAdvance[Symbol](t, LexicalElem.operatorFrom(_, TokenTypes.UNARY_OPERATORS))
          term <- compileTerm(t)
        } yield unaryOp +: term
      case _ => ???

    lexElemOrError match
      case s: String => Left(s)
      case elem: LexicalElem => Right(List(elem)).tap(_ => t.advance())
      case maybeList: MaybeLexicalElements => maybeList

  @tailrec
  private def compileOptionalStatements(t: Tokeniser, current: List[LexicalElem] = List()): MaybeLexicalElements =
    compileStatement(t) match
      case Left(_) => Right(current)
      case Right(s) => compileOptionalStatements(t, current ++ s)

  private def compileOptional(t: Tokeniser, stop: Tokeniser => Boolean, compile: Tokeniser => MaybeLexicalElements) = {
    @tailrec
    def go(current: List[LexicalElem]): MaybeLexicalElements = {
      if (stop(t))
        Right(current)
      else
        compile(t) match
          case Left(err) => Left(err)
          case Right(tokens) => go(current ++ tokens)
    }

    go(List())
  }

  private def compileOptionalSubroutines(t: Tokeniser, current: List[LexicalElem] = List()): MaybeLexicalElements =
    val stopFunction = (tokeniser: Tokeniser) => !TokenTypes.SUBROUTINE_TYPES.contains(tokeniser.currentToken)
    compileOptional(t, stopFunction, compileSubroutine)

  private def compileOptionalVarDecs(t: Tokeniser, classDec: Boolean = false, current: List[LexicalElem] = List()): MaybeLexicalElements =
    val stopFunction = (tokeniser: Tokeniser) => if (classDec)
      !TokenTypes.CLASS_VAR_TYPES.contains(tokeniser.currentToken)
    else
      tokeniser.currentToken != "var"
    compileOptional(t, stopFunction, compileVarDec(_, classDec))

  private def getOptionalVarParamList(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    t.currentToken match
      case ")" => Right(soFar).tap(_ => t.advance())
      case _ => getVarParamList(t, List())

  def compileIf(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "if")
      _ <- assertTokenEqualsAndAdvance(t, "(")
      expression <- compileExpression(t)
      _ <- assertTokenEqualsAndAdvance(t, ")")
      _ <- assertTokenEqualsAndAdvance(t, "{")
      statements <- compileOptionalStatements(t)
      _ <- assertTokenEqualsAndAdvance(t, "}")
      maybeElse <- if (t.hasMoreTokens && t.currentToken == "else") {
        for {
          _ <- assertTokenEqualsAndAdvance(t, "else")
          _ <- assertTokenEqualsAndAdvance(t, "{")
          statements <- compileOptionalStatements(t)
          _ <- assertTokenEqualsAndAdvance(t, "}")
        } yield Keyword("else") +: wrapCurlyBrackets(statements)
      } else {
      Right(List())
    }
    } yield (Keyword("if") +: wrapBrackets(expression)) ++ wrapCurlyBrackets(statements) ++ maybeElse

  def compileExpression(t: Tokeniser): Either[String, List[LexicalElem]] =
    for {
      term <- compileTerm(t)
      opTerm <- if (t.hasMoreTokens && TokenTypes.OPERATORS.contains(t.currentToken)) {
        for {
          operator <- getLexElementAsAndAdvance[Symbol](t, LexicalElem.operatorFrom(_, TokenTypes.OPERATORS))
          nextTerm <- compileTerm(t)
        } yield operator +: nextTerm
      } else
        Right(List())
    } yield term ++ opTerm

  private def getOptionalListOfVarsFollowedByClosingChar(t: Tokeniser, varListSoFar: List[LexicalElem] = List()): MaybeLexicalElements =
    t.currentToken match
      case ")" => Right(varListSoFar).tap(_ => t.advance())
      case _ => getVarList(t, List())

  private def getVarParamList(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    for {
      varType <- getLexElementAsAndAdvance[LexicalElem](t, LexicalElem.keywordOrIndentifierFrom) //TODO this can return an invalid type
      identifier <- getLexElementAsAndAdvance[Identifier](t, LexicalElem.identifierFrom)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar ++ List(varType, identifier)
      r <- if (continue)
        getVarParamList(t, newVarList :+ Symbol(','))
      else
        Right(newVarList)
    } yield r

  private def getVarDecList(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    for {
      lexElem <- getLexElementAsAndAdvance[Identifier](t, LexicalElem.identifierFrom)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(";", ","))
      continue = nextToken == ","
      newVarList = soFar :+ lexElem
      r <- if (continue)
        getVarDecList(t, newVarList :+ Symbol(','))
      else
        Right(newVarList)
    } yield r

  private def getVarList(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    for {
      lexElem <- compileExpression(t)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar ++ lexElem
      r <- if (continue)
        getVarList(t, newVarList :+ Symbol(','))
      else
        Right(newVarList)
    } yield r

  private def assertNextTokenEqualsOneOf(t: Tokeniser, equals: Set[String]): Either[String, String] =
    t.currentToken match
      case s if equals.contains(s) => Right(s).tap(_ => t.advance())
      case otherString => Left(s"uh-oh, expected $otherString to equal ${equals.toList.mkString(" or ")}")

  private def getLexElementAsAndAdvance[T <: LexicalElem](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def assertTokenEqualsAndAdvance(t: Tokeniser, equals: String): Either[String, Unit] =
    assertNextTokenEqualsOneOf(t, Set(equals)).map(_ => ())
}
