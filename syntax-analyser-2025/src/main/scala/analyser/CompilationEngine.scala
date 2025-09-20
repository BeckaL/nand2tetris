package analyser

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object CompilationEngine {
  private type MaybeLexicalElements = Either[String, List[LexicalElem]]

  def compileLet(t: Tokeniser): MaybeLexicalElements = {
    for {
      _ <- expectStringAndAdvance(t, "let")
      varName <- expectVarAndAdvance(t)
      _ <- expectStringAndAdvance(t, "=")
      term <- parseExpressionPartial(t)
      _ <- expectStringAndAdvance(t, ";")
    } yield encloseWithTags("letStatement", List(Keyword("let"), varName, Symbol('=')) ++ term ++ List(Symbol(';')))
  }

  def compileClass(t: Tokeniser): MaybeLexicalElements = {
    for {
      _ <- expectStringAndAdvance(t, "class")
      className <- expectVarAndAdvance(t)
      _ <- expectStringAndAdvance(t, "{")
      classVarDecs <- compileZeroOrMore(t, List("static", "field"), compileClassVarDec)
      subroutineDecs <- compileZeroOrMore(t, List("function", "method", "constructor"), compileSubroutine)
      closingToken <- if (t.currentToken == "}") {
        if (t.hasMoreTokens) {
          Left("uh oh tokens after class has closed")
        } else {
          Right(Symbol('}'))
        }
      } else {
        Left(s"expected closing char } for class")
      }
    } yield encloseWithTags("class", List(Keyword("class"), className, Symbol('{')) ++ classVarDecs ++ subroutineDecs :+ Symbol('}'))
  }

  @tailrec
  private def compileZeroOrMore(t: Tokeniser, startTokens: List[String], transformer: Tokeniser => MaybeLexicalElements, soFar: List[LexicalElem] = List(), enclosingTag: Option[String] = None): MaybeLexicalElements =
    if (!startTokens.contains(t.currentToken)) {
      enclosingTag match {
        case Some(tag) => Right(encloseWithTags(tag, soFar))
        case None => Right(soFar)
      }
    } else {
      val result = transformer(t)
      result match {
        case Left(err) => Left(err)
        case Right(newElems) => compileZeroOrMore(t, startTokens, transformer, soFar ++ newElems, enclosingTag)
      }
    }

  def compileClassVarDec(t: Tokeniser): MaybeLexicalElements =
    for {
      staticOrField <- expectOneOfAndAdvance(t, List("static", "field"), s => Keyword(s))
      varType <- expectTypeAndAdvance(t)
      varNameLexElems <- parseNVars(t, List())
      _ <- expectStringAndAdvance(t, ";")
    } yield encloseWithTags("classVarDec", List(staticOrField, varType) ++ varNameLexElems ++ List(Symbol(';')))


  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = {
    for {
      subroutineType <- expectOneOfAndAdvance(t, List("function", "method", "constructor"), Keyword)
      returnType <- expectTypeAndAdvance(t, includeVoid = true)
      subroutineName <- expectVarAndAdvance(t)
      _ <- expectStringAndAdvance(t, "(")
      paramList <- compileParameterList(t, ")", List())
      _ <- expectStringAndAdvance(t, ")")
      subroutineBody <- compileSubroutineBody(t)
    } yield encloseWithTags("subroutineDec", List(subroutineType, returnType, subroutineName, Symbol('(')) ++ (paramList :+ Symbol(')')) ++ subroutineBody)
  }

  @tailrec
  def compileParameterList(t: Tokeniser, closingChar: String, elemsSoFar: List[LexicalElem] = List()): MaybeLexicalElements =
    if (t.currentToken == closingChar) {
      Right(encloseWithTags("parameterList", elemsSoFar))
    } else {
      val result = for {
        varType <- expectTypeAndAdvance(t)
        varNameLexElems <- expectVarAndAdvance(t)
      } yield List(varType, varNameLexElems)
      result match {
        case Left(str) => Left(str)
        case Right(newElems) =>
          if (t.currentToken == closingChar) {
            Right(encloseWithTags("parameterList", elemsSoFar ++ newElems))
          } else if (t.currentToken != ",") {
            Left(s"expected continuation of param list with comma, got ${t.currentToken}")
          } else {
            safeAdvance(t) match {
              case Left(err) => Left(err)
              case _ => compileParameterList(t, closingChar, elemsSoFar ++ (newElems :+ Symbol(',')))
            }
          }
      }
    }


  def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "{")
      varDecs <- compileVarDecs(t, List()) //TODO 0 or more
      statements <- compileStatements(t, "}", List())
      _ <- expectStringAndAdvance(t, "}")
    } yield encloseWithTags("subroutineBody", Symbol('{') +: (varDecs ++ statements :+ Symbol('}')))

  private def compileVarDecs(t: Tokeniser, soFar: List[LexicalElem]): MaybeLexicalElements =
    if (t.currentToken == "var") {
      compileVarDec(t).flatMap(newElems => compileVarDecs(t, soFar ++ newElems))
    } else {
      Right(soFar)
    }

  private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
    StartElem(tagname) +: elems :+ EndElem(tagname)

  def compileVarDec(t: Tokeniser): MaybeLexicalElements =
    for {
      varString <- expectStringAndAdvance(t, "var")
      varType <- expectTypeAndAdvance(t)
      varNameLexElems <- parseNVars(t, List())
      _ <- expectStringAndAdvance(t, ";")
    } yield encloseWithTags("varDec", List(Keyword("var"), varType) ++ varNameLexElems ++ List(Symbol(';')))

  //TODO write tests
  @tailrec
  def compileStatements(t: Tokeniser, terminatingString: String, soFar: List[LexicalElem]): MaybeLexicalElements = {
    val validStatementStarts = List("let", "do", "while", "if", "return")
    t.currentToken match {
      case s if s == terminatingString => Right(encloseWithTags("statements", soFar))
      case nonStatementString if !validStatementStarts.contains(nonStatementString) =>
        Left(s"uh oh, tried to compile a statement starting with ${nonStatementString}")
      case statementKeyword if validStatementStarts.contains(statementKeyword) =>
        val newKeywords = statementKeyword match {
          case "let" => compileLet(t)
          case "do" => compileDo(t)
          case "while" => compileWhile(t)
          case "if" => compileIf(t)
          case _ => compileReturn(t)
        }
        newKeywords match {
          case Left(err) => Left(err)
          case Right(newElems) => compileStatements(t, terminatingString, soFar ++ newElems)
        }
    }
  }

  def compileIf(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "if")
      _ <- expectStringAndAdvance(t, "(")
      exp <- parseExpressionPartial(t)
      _ <- expectStringAndAdvance(t, ")")
      statementsWithCurlyBrackets <- expectStatementsEnclosedByCurlyBrackets(t)
      optionalElse <-
        if (t.currentToken == "else")
          safeAdvance(t)
            .flatMap(_ => expectStatementsEnclosedByCurlyBrackets(t))
            .map(statementWithCurlies => Keyword("else") +: statementWithCurlies)
        else
          Right(List())
    } yield encloseWithTags("ifStatement", List(Keyword("if"), Symbol('(')) ++ exp ++ List(Symbol(')')) ++ statementsWithCurlyBrackets ++ optionalElse)

  private def expectStatementsEnclosedByCurlyBrackets(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "{")
      statements <- compileStatements(t, "}", List())
      _ <- expectStringAndAdvance(t, "}")
    } yield Symbol('{') +: (statements :+ Symbol('}'))

  def compileWhile(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "while")
      _ <- expectStringAndAdvance(t, "(")
      exp <- parseExpressionPartial(t)
      _ <- expectStringAndAdvance(t, ")")
      statementsWithCurlies <- expectStatementsEnclosedByCurlyBrackets(t)
    } yield encloseWithTags("whileStatement", List(Keyword("while"), Symbol('(')) ++ exp ++ List(Symbol(')')) ++ statementsWithCurlies)

  def compileDo(t: Tokeniser): MaybeLexicalElements = {
    for {
      _ <- expectStringAndAdvance(t, "do")
      subroutineCall <- parseSubroutineCall(t)
      _ <- expectStringAndAdvance(t, ";")
    } yield encloseWithTags("doStatement", Keyword("do") +: subroutineCall :+ Symbol(';'))
  }

  private def parseSubroutineCall(t: Tokeniser) =
    for {
      v1 <- expectVarAndAdvance(t)
      optionalDotCall <- if (t.currentToken == ".") {
        safeAdvance(t).flatMap(_ => expectVarAndAdvance(t).map(v2 => List(Symbol('.'), v2)))
      } else if (t.currentToken == "(") {
        Right(List())
      } else {
        Left("expected a valid subroutine call")
      }
      _ <- expectStringAndAdvance(t, "(")
      (expressionList, _) <- compileExpressionList(t)
      _ <- expectStringAndAdvance(t, ")")
    } yield v1 +: (optionalDotCall ++ (Symbol('(') +: expressionList :+ Symbol(')')))

  //TODO: get rid
  private def parseExpressionPartial(t: Tokeniser) = expectTerm(t).map(elems => encloseWithTags("expression", elems))

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "return")
      optionalExpression <-
        if (t.currentToken != ";") {
          parseExpressionPartial(t).map(Some(_)) //TODO expect expression when expressions implemented
        } else Right(Option.empty)
      _ <- expectStringAndAdvance(t, ";")
    } yield encloseWithTags("returnStatement", List(Keyword("return")) ++ (optionalExpression.toList.flatten :+ Symbol(';')))

  //TODO part 2
  def compileExpression(t: Tokeniser): MaybeLexicalElements = ???

  def compileExpressionList(t: Tokeniser): Either[String, (List[LexicalElem], Int)] =
    def go(soFar: List[LexicalElem], expressionListCount: Int): Either[String, (List[LexicalElem], Int)] = {
      if (t.currentToken == ")") //covers case of empty list
        Right(encloseWithTags("expressionList", soFar), expressionListCount)
      else
        //TODO sub this out for real compile expression when expressions fully implemented
        parseExpressionPartial(t).flatMap(lexElems =>
          if (t.currentToken == ",") {
            safeAdvance(t).flatMap(_ => go(soFar ++ lexElems :+ Symbol(','), expressionListCount + 1))
          } else if (t.currentToken == ")") {
            Right(encloseWithTags("expressionList",soFar ++ lexElems), expressionListCount + 1)
          } else {
            Left(s"invalid expression list, expected , or ) but got ${t.currentToken}")
          }
        )
    }

    go(List(), 0)

  //TODO
  def compileTerm(t: Tokeniser): MaybeLexicalElements = ???

  private def expectTerm(t: Tokeniser): MaybeLexicalElements =
    val s = t.currentToken
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(encloseWithTags("term", List(Identifier(s)))).tap(_ => t.advance())
      case TokenTypes.IntConst => Right(encloseWithTags("term", List(IntConst(s.toInt)))).tap(_ => t.advance())
      case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Right(encloseWithTags("term", List(Keyword(s)))).tap(_ => t.advance())
      case TokenTypes.StringConst => Right(encloseWithTags("term", List(stringConstFromQuotedString(s)))).tap(_ => t.advance())
      case otherTokenType => Left(s"expected valid term to be found in string $s but got $otherTokenType")

  private def expectVarAndAdvance(t: Tokeniser): Either[String, LexicalElem] =
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

  private def expectTypeAndAdvance(t: Tokeniser, includeVoid: Boolean = false): Either[String, LexicalElem] =
    val currentToken = t.currentToken
    TokenTypes.tokenType(currentToken) match {
      case TokenTypes.Keyword =>
        val variableTypes = List("boolean", "char", "int")
        val typesToCheck = if (includeVoid) "void" +: variableTypes else variableTypes
        if (typesToCheck.contains(currentToken))
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
    expectVarAndAdvance(t).flatMap(varNameLexElem =>
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
