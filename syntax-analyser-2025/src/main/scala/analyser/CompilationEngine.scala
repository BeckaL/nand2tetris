package analyser

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object CompilationEngine {
  def compileLet(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = optionalElemRule(_ == "[", List(symbolRule("["), compileExpression, symbolRule("]")), false)
    val rules = List(keywordRule("let"), varRule, optionalExpressionRule, symbolRule("="), compileExpression, semicolon)
    compileWithRules(t, rules, Some("letStatement"))

  def compileClass(t: Tokeniser): MaybeLexicalElements = {
    val terminatingClosingBracket = (tk: Tokeniser) => {
      val moreTokensWarning = "uh oh tokens after class has closed"
      if (tk.currentToken == "}")
        if (tk.hasMoreTokens) Left(moreTokensWarning) else Right(List(Symbol('}')))
      else
        Left(s"expected closing char } for class")
    }
    val rules = List(
      keywordRule("class"),
      varRule,
      openCurlyBracket,
      zeroOrMoreRule(List("static", "field"), compileClassVarDec),
      zeroOrMoreRule(List("function", "method", "constructor"), compileSubroutine),
      terminatingClosingBracket
    )
    compileWithRules(t, rules, Some("class"))
  }

  def compileClassVarDec(t: Tokeniser): MaybeLexicalElements =
    val rules =
      List(keywordMatchingOneOfRule(List("static", "field")), typeRule(), parseNVars(_), semicolon)
    compileWithRules(t, rules, Some("classVarDec"))

  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = {
    val rules = List(
      keywordMatchingOneOfRule(List("function", "method", "constructor")),
      typeRule(includeVoid = true),
      varRule,
      openBracket,
      compileParameterList(_, ")"),
      closeBracket,
      compileSubroutineBody
    )
    compileWithRules(t, rules, Some("subroutineDec"))
  }

  @tailrec
  def compileParameterList(t: Tokeniser, closingChar: String, elemsSoFar: List[LexicalElem] = List()): MaybeLexicalElements =
    if (t.currentToken == closingChar) {
      Right(encloseWithTags("parameterList", elemsSoFar))
    } else {
      val result = for {
        varType <- typeRule(includeVoid = true)(t)
        varNameLexElems <- varRule(t)
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
    val rules = List(openCurlyBracket, zeroOrMoreRule(List("var"), compileVarDec), compileStatements(_, "}"), closeCurlyBracket)
    compileWithRules(t, rules, Some("subroutineBody"))

  private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
    StartElem(tagname) +: elems :+ EndElem(tagname)

  def compileVarDec(t: Tokeniser): MaybeLexicalElements =
    val rules = List(keywordRule("var"), typeRule(), parseNVars(_), semicolon)
    compileWithRules(t, rules, Some("varDec"))

  //TODO write tests
  def compileStatements(t: Tokeniser, terminatingString: String, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    val ruleMap =
      Map("let" -> compileLet, "do" -> compileDo, "while" -> compileWhile, "if" -> compileIf, "return" -> compileReturn)
    compileWithRules(t, List(zeroOrMoreRule(ruleMap.keys.toList, branchingRule(ruleMap))), Some("statements"))

  def compileIf(t: Tokeniser): MaybeLexicalElements =
    val rules = List(
      keywordRule("if"),
      openBracket,
      compileExpression,
      closeBracket,
      compileWithRules(_, statementsEnclosedByCurlies, None),
      optionalElemRule(_ == "else", List((tk: Tokeniser) => compileWithRules(tk, statementsEnclosedByCurlies, None).map(elems => Keyword("else") +: elems)))
    )
    compileWithRules(t, rules, Some("ifStatement"))

  val statementsEnclosedByCurlies = List(openCurlyBracket, compileStatements(_, "}"), closeCurlyBracket)

  def compileWhile(t: Tokeniser): MaybeLexicalElements =
    val rules = List(keywordRule("while"), openBracket, compileExpression, closeBracket) ++ statementsEnclosedByCurlies
    compileWithRules(t, rules, Some("whileStatement"))

  def compileDo(t: Tokeniser): MaybeLexicalElements = {
    val doRules = List(keywordRule("do"), parseSubroutineCall, semicolon)
    compileWithRules(t, doRules, Some("doStatement"))
  }

  val subroutineCallRules =
    List(varRule, optionalElemRule(_ == ".", List(symbolRule("."), varRule)), openBracket, expressionListRule, closeBracket)

  private def parseSubroutineCall(t: Tokeniser) =
    compileWithRules(t, subroutineCallRules, None)

  private def parseSubroutineCallFromDot(t: Tokeniser, v: LexicalElem): MaybeLexicalElements =
    compileWithRules(t, subroutineCallRules.tail, None).map(elems => v +: elems)

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = optionalElemRule(_ != ";", List(compileExpression), advanceIfConditionMet = false)

    val rules = List(keywordRule("return"), optionalExpressionRule, semicolon)
    compileWithRules(t, rules, Some("returnStatement"))

  def compileExpression(t: Tokeniser): MaybeLexicalElements =
    val rules = List(
      compileTerm,
      optionalElemRule(TokenTypes.OPERATORS.contains, List(symbolMatchingOneOfRule(TokenTypes.OPERATORS.toList), compileTerm), false))
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

  val compileTermRule: RuleTransformer =
    (t: Tokeniser) => {
      val s = t.currentToken
      TokenTypes.tokenType(s) match
        case TokenTypes.Identifier =>
          val currentElem = Identifier(s)
          t.safeAdvance.flatMap(_ =>
            t.currentToken match {
              case currentToken if List("(", ".").contains(currentToken) => parseSubroutineCallFromDot(t, currentElem)
              case "[" =>
                val rules = List(symbolRule("["), compileExpression, symbolRule("]"))
                compileWithRules(t, rules, None).map(elems => currentElem +: elems)
              case _ => Right(List(currentElem))
            }
          )
        case TokenTypes.IntConst => Right(List(IntConst(s.toInt))).tap(_ => t.advance())
        case TokenTypes.Keyword if TokenTypes.KEYWORD_CONSTANTS.contains(s) => Right(List(Keyword(s))).tap(_ => t.advance())
        case TokenTypes.StringConst => Right(List(stringConstFromQuotedString(s))).tap(_ => t.advance())
        case TokenTypes.Symbol if TokenTypes.UNARY_OPERATORS.contains(s) =>
          compileWithRules(t, List(symbolMatchingOneOfRule(TokenTypes.UNARY_OPERATORS.toList), compileTerm), None)
        case TokenTypes.Symbol if s == "(" =>
          val rules = List(symbolRule("("), compileExpression, symbolRule(")"))
          compileWithRules(t, rules, None)
        case otherTokenType => Left(s"expected valid term to be found in string $s but got $otherTokenType")
    }

  def compileTerm(t: Tokeniser): MaybeLexicalElements =
    compileWithRules(t, List(compileTermRule), Some("term"))

  private def stringConstFromQuotedString(s: String): StringConst =
    StringConst(s.tail.dropRight(1))

  private def parseNVars(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    varRule(t).flatMap(varNameLexElem =>
      t.currentToken match {
        case ";" => Right(soFar ++ varNameLexElem)
        case "," => t.safeAdvance.flatMap(_ =>
          parseNVars(t, soFar ++ varNameLexElem ++ List(Symbol(',')))
        )
        case otherToken => Left(s"expected either ';' or ',' when parsing vars, got $otherToken")
      })
}
