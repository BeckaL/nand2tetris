package analyser

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object CompilationEngine {
  def compileLet(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("let"), VarRule, SymbolRule("="), CustomRule(parseExpressionPartial), semicolon)
    compileWithRules(t, rules, Some("letStatement"))

  def compileClass(t: Tokeniser): MaybeLexicalElements = {
    val terminatingClosingBracket = CustomRule(tk =>
      val moreTokensWarning = "uh oh tokens after class has closed"
      if (tk.currentToken == "}")
        if (tk.hasMoreTokens) Left(moreTokensWarning) else Right(List(Symbol('}')))
      else
        Left(s"expected closing char } for class")
    )
    val rules = List(
      KeywordRule("class"),
      VarRule,
      openCurlyBracket,
      ZeroOrMoreRule(List("static", "field"), compileClassVarDec),
      ZeroOrMoreRule(List("function", "method", "constructor"), compileSubroutine),
      terminatingClosingBracket
    )
    compileWithRules(t, rules, Some("class"))
  }



  def compileClassVarDec(t: Tokeniser): MaybeLexicalElements =
    val rules =
      List(KeywordMatchingOneOfRule(List("static", "field")), TypeRule(), CustomRule(parseNVars(_)), semicolon)
    compileWithRules(t, rules, Some("classVarDec"))

  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = {
    val rules = List(
      KeywordMatchingOneOfRule(List("function", "method", "constructor")),
      TypeRule(includeVoid = true),
      VarRule,
      openBracket,
      CustomRule(compileParameterList(_, ")")),
      closeBracket,
      CustomRule(compileSubroutineBody)
    )
    compileWithRules(t, rules, Some("subroutineDec"))
  }

  @tailrec
  def compileParameterList(t: Tokeniser, closingChar: String, elemsSoFar: List[LexicalElem] = List()): MaybeLexicalElements =
    if (t.currentToken == closingChar) {
      Right(encloseWithTags("parameterList", elemsSoFar))
    } else {
      val result = for {
        varType <- expectTypeAndAdvance(t)
        varNameLexElems <- expectVarAndAdvance(t)
      } yield varType ++ varNameLexElems
      result match {
        case Left(str) => Left(str)
        case Right(newElems) =>
          if (t.currentToken == closingChar) {
            Right(encloseWithTags("parameterList", elemsSoFar ++ newElems))
          } else if (t.currentToken != ",") {
            Left(s"expected continuation of param list with comma, got ${t.currentToken}")
          } else {
            t.safeAdvance match {
              case Left(err) => Left(err)
              case _ => compileParameterList(t, closingChar, elemsSoFar ++ (newElems :+ Symbol(',')))
            }
          }
      }
    }

  def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements =
    val rules = List(openCurlyBracket, ZeroOrMoreRule(List("var"), compileVarDec), CustomRule(compileStatements(_, "}")), closeCurlyBracket)
    compileWithRules(t, rules, Some("subroutineBody"))

  private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
    StartElem(tagname) +: elems :+ EndElem(tagname)

  def compileVarDec(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("var"), TypeRule(), CustomRule(parseNVars(_)), semicolon)
    compileWithRules(t, rules, Some("varDec"))

  //TODO write tests
  @tailrec
  def compileStatements(t: Tokeniser, terminatingString: String, soFar: List[LexicalElem] = List()): MaybeLexicalElements = {
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
    val rules = List(
      KeywordRule("if"),
      openBracket,
      CustomRule(parseExpressionPartial),
      closeBracket,
      statementsEnclosedByCurlies,
      OptionalElemRule(_ == "else", (t: Tokeniser) => statementsEnclosedByCurlies.compile(t).map(Keyword("else") +: _))
    )
    compileWithRules(t, rules, Some("ifStatement"))

  val statementsEnclosedByCurlies = ObjectRule(List(openCurlyBracket, CustomRule(compileStatements(_, "}")), closeCurlyBracket))

  def compileWhile(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("while"), openBracket, CustomRule(parseExpressionPartial), closeBracket, statementsEnclosedByCurlies)
    compileWithRules(t, rules, Some("whileStatement"))

  def compileDo(t: Tokeniser): MaybeLexicalElements = {
    val doRules = List(KeywordRule("do"), ObjectRule(subroutineCallRules), semicolon)
    compileWithRules(t, doRules, Some("doStatement"))
  }

  val optionalDotCallRule = (t: Tokeniser) =>
    if (t.currentToken == ".") {
      t.safeAdvance.flatMap(_ => expectVarAndAdvance(t).map(v2 => Symbol('.') +: v2))
    } else if (t.currentToken == "(") {
      Right(List())
    } else {
      Left("expected a valid subroutine call")
    }

  val subroutineCallRules =
    List(VarRule, CustomRule(optionalDotCallRule), openBracket, ExpressionListRule, closeBracket)

  private def parseSubroutineCall(t: Tokeniser) =
    compileWithRules(t, subroutineCallRules, None)

  //TODO: get rid
  private def parseExpressionPartial(t: Tokeniser) = expectTerm(t).map(elems => encloseWithTags("expression", elems))

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = OptionalElemRule(_ != ";", parseExpressionPartial, advanceIfConditionMet = false)

    val rules = List(KeywordRule("return"), optionalExpressionRule, semicolon)
    compileWithRules(t, rules, Some("returnStatement"))

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
            t.safeAdvance.flatMap(_ => go(soFar ++ lexElems :+ Symbol(','), expressionListCount + 1))
          } else if (t.currentToken == ")") {
            Right(encloseWithTags("expressionList", soFar ++ lexElems), expressionListCount + 1)
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

  private def expectVarAndAdvance(t: Tokeniser): MaybeLexicalElements =
    VarRule.compile(t) //TODO legacy

  private def expectTypeAndAdvance(t: Tokeniser, includeVoid: Boolean = false): MaybeLexicalElements =
    TypeRule(includeVoid).compile(t) //TODO legacy

  private def stringConstFromQuotedString(s: String): StringConst =
    StringConst(s.tail.dropRight(1))

  private def parseNVars(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    expectVarAndAdvance(t).flatMap(varNameLexElem =>
      t.currentToken match {
        case ";" => Right(soFar ++ varNameLexElem)
        case "," => t.safeAdvance.flatMap(_ =>
          parseNVars(t, soFar ++ varNameLexElem ++ List(Symbol(',')))
        )
        case otherToken => Left(s"expected either ';' or ',' when parsing vars, got $otherToken")
      })
}
