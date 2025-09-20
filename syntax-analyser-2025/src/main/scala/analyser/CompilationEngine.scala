package analyser

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object CompilationEngine {
  def compileLet(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = OptionalElemRule(_ == "[", ObjectRule(List(SymbolRule("["), CustomRule(compileExpression), SymbolRule("]"))).compile, false)
    val rules = List(KeywordRule("let"), VarRule, optionalExpressionRule, SymbolRule("="), CustomRule(compileExpression), semicolon)
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
  def compileStatements(t: Tokeniser, terminatingString: String, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    val ruleMap =
      Map("let" -> compileLet, "do" -> compileDo, "while" -> compileWhile, "if" -> compileIf, "return" -> compileReturn)
    compileWithRules(t, List(ZeroOrMoreRule(ruleMap.keys.toList, BranchingRule(ruleMap).compile)), Some("statements"))

  def compileIf(t: Tokeniser): MaybeLexicalElements =
    val rules = List(
      KeywordRule("if"),
      openBracket,
      CustomRule(compileExpression),
      closeBracket,
      statementsEnclosedByCurlies,
      OptionalElemRule(_ == "else", (t: Tokeniser) => statementsEnclosedByCurlies.compile(t).map(Keyword("else") +: _))
    )
    compileWithRules(t, rules, Some("ifStatement"))

  val statementsEnclosedByCurlies = ObjectRule(List(openCurlyBracket, CustomRule(compileStatements(_, "}")), closeCurlyBracket))

  def compileWhile(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("while"), openBracket, CustomRule(compileExpression), closeBracket, statementsEnclosedByCurlies)
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

  private def parseSubroutineCallFromDot(t: Tokeniser, v: LexicalElem): MaybeLexicalElements =
    compileWithRules(t, subroutineCallRules.tail, None).map(elems => v +: elems)

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = OptionalElemRule(_ != ";", CustomRule(compileExpression).compile, advanceIfConditionMet = false)

    val rules = List(KeywordRule("return"), optionalExpressionRule, semicolon)
    compileWithRules(t, rules, Some("returnStatement"))

  def compileExpression(t: Tokeniser): MaybeLexicalElements =
    val rules = List(
      CustomRule(compileTerm),
      OptionalElemRule(TokenTypes.OPERATORS.contains, ObjectRule(List(SymbolMatchingOneOfRule(TokenTypes.OPERATORS.toList), CustomRule(compileTerm))).compile, false))
    compileWithRules(t, rules, Some("expression"))

  def compileExpressionList(t: Tokeniser): Either[String, (List[LexicalElem], Int)] =
    def go(soFar: List[LexicalElem], expressionListCount: Int): Either[String, (List[LexicalElem], Int)] = {
      if (t.currentToken == ")") //covers case of empty list
        Right(encloseWithTags("expressionList", soFar), expressionListCount)
      else
        compileExpression(t).flatMap(lexElems =>
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

  val compileTermRule: CompilationRule = CustomRule(
    (t: Tokeniser) =>
      val s = t.currentToken
      TokenTypes.tokenType(s) match
        case TokenTypes.Identifier =>
          val currentElem = Identifier(s)
          t.safeAdvance.flatMap(_ =>
            t.currentToken match {
              case currentToken if List("(", ".").contains(currentToken) => parseSubroutineCallFromDot(t, currentElem)
              case "[" =>
                val rules = List(SymbolRule("["), CustomRule(compileExpression), SymbolRule("]"))
                compileWithRules(t, rules, None).map(elems => currentElem +: elems)
              case _ => Right(List(currentElem))
            }
          )
        case TokenTypes.IntConst => Right(List(IntConst(s.toInt))).tap(_ => t.advance())
        case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Right(List(Keyword(s))).tap(_ => t.advance())
        case TokenTypes.StringConst => Right(List(stringConstFromQuotedString(s))).tap(_ => t.advance())
        case TokenTypes.Symbol if TokenTypes.UNARY_OPERATORS.contains(s) =>
          compileWithRules(t, List(SymbolMatchingOneOfRule(TokenTypes.UNARY_OPERATORS.toList), CustomRule(compileTerm)), None)
        case TokenTypes.Symbol if s == "(" =>
          val rules = List(SymbolRule("("), CustomRule(compileExpression), SymbolRule(")"))
          compileWithRules(t, rules, None)
        case otherTokenType => Left(s"expected valid term to be found in string $s but got $otherTokenType")
  )

  def compileTerm(t: Tokeniser): MaybeLexicalElements =
    compileWithRules(t, List(compileTermRule), Some("term"))

  private def expectTerm(t: Tokeniser): MaybeLexicalElements =
    val s = t.currentToken
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => Right(encloseWithTags("term", List(Identifier(s)))).tap(_ => t.advance())
      case TokenTypes.IntConst => Right(encloseWithTags("term", List(IntConst(s.toInt)))).tap(_ => t.advance())
      case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Right(encloseWithTags("term", List(Keyword(s)))).tap(_ => t.advance())
      case TokenTypes.StringConst => Right(encloseWithTags("term", List(stringConstFromQuotedString(s)))).tap(_ => t.advance())
      case TokenTypes.Symbol if s == "(" =>
        val rules = List(SymbolRule("("), CustomRule(compileExpression), SymbolRule(")"))
        compileWithRules(t, rules, None)
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
