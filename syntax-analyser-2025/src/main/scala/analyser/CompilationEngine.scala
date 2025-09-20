package analyser

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object CompilationEngine {
  private type MaybeLexicalElements = Either[String, List[LexicalElem]]

  def compileLet(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("let"), VarRule, SymbolRule("="), CustomRule(parseExpressionPartial), SymbolRule(";"))
    compileWithRules(t, rules, Some("letStatement"))

  def compileClass(t: Tokeniser): MaybeLexicalElements = {
    val terminatingClosingBracket = CustomRule(tk =>
      if (tk.currentToken == "}") {
        if (tk.hasMoreTokens) {
          Left("uh oh tokens after class has closed")
        } else {
          Right(List(Symbol('}')))
        }
      } else {
        Left(s"expected closing char } for class")
      })
    val rules = List(
      KeywordRule("class"),
      VarRule,
      SymbolRule("{"),
      CustomRule(compileZeroOrMore(_, List("static", "field"), compileClassVarDec)),
      CustomRule(compileZeroOrMore(_, List("function", "method", "constructor"), compileSubroutine)),
      terminatingClosingBracket
    )
    compileWithRules(t, rules, Some("class"))
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
    val rules =
      List(StringMatchingOneOfRule(List("static", "field"), Keyword.apply), CustomRule(expectTypeAndAdvance(_)), CustomRule(parseNVars(_)), SymbolRule(";"))
    compileWithRules(t, rules, Some("classVarDec"))

  def compileSubroutine(t: Tokeniser): MaybeLexicalElements = {
    for {
      subroutineType <- expectOneOfAndAdvance(t, List("function", "method", "constructor"), Keyword)
      returnType <- expectTypeAndAdvance(t, includeVoid = true)
      subroutineName <- expectVarAndAdvance(t)
      _ <- expectStringAndAdvance(t, "(")
      paramList <- compileParameterList(t, ")", List())
      _ <- expectStringAndAdvance(t, ")")
      subroutineBody <- compileSubroutineBody(t)
    } yield encloseWithTags("subroutineDec", List(subroutineType) ++ returnType ++ List(subroutineName, Symbol('(')) ++ (paramList :+ Symbol(')')) ++ subroutineBody)
  }

  @tailrec
  def compileParameterList(t: Tokeniser, closingChar: String, elemsSoFar: List[LexicalElem] = List()): MaybeLexicalElements =
    if (t.currentToken == closingChar) {
      Right(encloseWithTags("parameterList", elemsSoFar))
    } else {
      val result = for {
        varType <- expectTypeAndAdvance(t)
        varNameLexElems <- expectVarAndAdvance(t)
      } yield varType ++ List(varNameLexElems)
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
    val rules = List(SymbolRule("{"), CustomRule(compileVarDecs(_)), CustomRule(compileStatements(_, "}", List())), SymbolRule("}"))
    compileWithRules(t, rules, Some("subroutineBody"))

  private def compileVarDecs(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
    if (t.currentToken == "var") {
      compileVarDec(t).flatMap(newElems => compileVarDecs(t, soFar ++ newElems))
    } else {
      Right(soFar)
    }

  private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
    StartElem(tagname) +: elems :+ EndElem(tagname)

  def compileVarDec(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("var"), CustomRule(expectTypeAndAdvance(_)), CustomRule(parseNVars(_)), SymbolRule(";"))
    compileWithRules(t, rules, Some("varDec"))

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
    val rules = List(
      KeywordRule("if"),
      openBracket,
      CustomRule(parseExpressionPartial),
      closeBracket,
      CustomRule(expectStatementsEnclosedByCurlyBrackets),
      OptionalElemRule(_ == "else", (t: Tokeniser) => expectStatementsEnclosedByCurlyBrackets(t).map(Keyword("else") +: _))
    )
    compileWithRules(t, rules, Some("ifStatement"))

  case class OptionalElemRule(condition: String => Boolean, rule: Tokeniser => MaybeLexicalElements) extends CompilationRule {
    override def compile(t: Tokeniser) =
      if (condition(t.currentToken))
        safeAdvance(t).flatMap(_ => rule(t))
      else
        Right(List())
  }

  private def expectStatementsEnclosedByCurlyBrackets(t: Tokeniser): MaybeLexicalElements =
    for {
      _ <- expectStringAndAdvance(t, "{")
      statements <- compileStatements(t, "}", List())
      _ <- expectStringAndAdvance(t, "}")
    } yield Symbol('{') +: (statements :+ Symbol('}'))

  val openBracket = SymbolRule("(")
  val closeBracket = SymbolRule(")")

  def compileWhile(t: Tokeniser): MaybeLexicalElements =
    val rules = List(KeywordRule("while"), openBracket, CustomRule(parseExpressionPartial), closeBracket, CustomRule(expectStatementsEnclosedByCurlyBrackets))
    compileWithRules(t, rules, Some("whileStatement"))

  def compileDo(t: Tokeniser): MaybeLexicalElements = {
    val doRules = List(KeywordRule("do"), ObjectRule(subroutineCallRules), SymbolRule(";"))
    compileWithRules(t, doRules, Some("doStatement"))
  }

  def compileWithRules(t: Tokeniser, rules: List[CompilationRule], enclosingElem: Option[String]): MaybeLexicalElements =
    @tailrec
    def go(soFar: List[LexicalElem], remainingRules: List[CompilationRule]): MaybeLexicalElements =
      remainingRules match
        case Nil =>
          enclosingElem match {
            case Some(elem) => Right(encloseWithTags(elem, soFar))
            case None => Right(soFar)
          }
        case firstRule :: otherRules =>
          firstRule.compile(t) match
            case Left(err) => Left(err)
            case Right(newElems) => go(soFar ++ newElems, otherRules)

    go(List(), rules)

  trait CompilationRule {
    def compile(t: Tokeniser): MaybeLexicalElements
  }

  case class KeywordRule(stringToMatch: String) extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements =
      stringMatchingRule(t, List(stringToMatch), Keyword.apply)
  }

  case class SymbolRule(stringToMatch: String) extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements =
      stringMatchingRule(t, List(stringToMatch), (s: String) => Symbol(s.head))
  }

  private def stringMatchingRule(t: Tokeniser, stringsToMatch: List[String], transformer: String => LexicalElem): MaybeLexicalElements =
    val s = t.currentToken
    if (stringsToMatch.contains(s))
      safeAdvance(t).map(_ => List(transformer(s)))
    else
      Left(s"expected one of $stringsToMatch, got ${t.currentToken}")

  case class ObjectRule(rules: List[CompilationRule], enclosingElem: Option[String] = None) extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements =
      compileWithRules(t, rules, enclosingElem)
  }

  case class StringMatchingOneOfRule(stringsToMatch: List[String], transformer: String => LexicalElem) extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements =
      stringMatchingRule(t, stringsToMatch, transformer)
  }

  case object VarRule extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements =
      val s = t.currentToken
      TokenTypes.tokenType(s) match
        case TokenTypes.Identifier => safeAdvance(t).flatMap(_ => Right(List(Identifier(s))))
        case otherTokenType => Left(s"expected token type identifier for string $s but got $otherTokenType")
  }

  case class CustomRule(rule: Tokeniser => MaybeLexicalElements) extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements = rule(t)
  }

  case object ExpressionListRule extends CompilationRule {
    override def compile(t: Tokeniser): MaybeLexicalElements = compileExpressionList(t).map(_._1)
  }

  val optionalDotCallRule = (t: Tokeniser) =>
    if (t.currentToken == ".") {
      safeAdvance(t).flatMap(_ => expectVarAndAdvance(t).map(v2 => List(Symbol('.'), v2)))
    } else if (t.currentToken == "(") {
      Right(List())
    } else {
      Left("expected a valid subroutine call")
    }

  val subroutineCallRules =
    List(VarRule, CustomRule(optionalDotCallRule), SymbolRule("("), ExpressionListRule, SymbolRule(")"))

  private def parseSubroutineCall(t: Tokeniser) =
    compileWithRules(t, subroutineCallRules, None)

  //TODO: get rid
  private def parseExpressionPartial(t: Tokeniser) = expectTerm(t).map(elems => encloseWithTags("expression", elems))

  def compileReturn(t: Tokeniser): MaybeLexicalElements =
    val optionalExpressionRule = (tk: Tokeniser) =>
      if (t.currentToken != ";") {
        parseExpressionPartial(t) //TODO expect expression when expressions implemented
      } else Right(List())

    val rules = List(KeywordRule("return"), CustomRule(optionalExpressionRule), SymbolRule(";"))
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
            safeAdvance(t).flatMap(_ => go(soFar ++ lexElems :+ Symbol(','), expressionListCount + 1))
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

  private def expectTypeAndAdvance(t: Tokeniser, includeVoid: Boolean = false): MaybeLexicalElements =
    val currentToken = t.currentToken
    TokenTypes.tokenType(currentToken) match {
      case TokenTypes.Keyword =>
        val variableTypes = List("boolean", "char", "int")
        val typesToCheck = if (includeVoid) "void" +: variableTypes else variableTypes
        if (typesToCheck.contains(currentToken))
          safeAdvance(t).flatMap(_ => Right(List(Keyword(currentToken))))
        else
          Left(s"keyword $currentToken cannot be used as a type, valid keyword types are boolean char or int")
      case TokenTypes.Identifier =>
        Right(List(Identifier(currentToken))).tap(_ => t.advance())
      case other =>
        Left(s"$currentToken cannot be used as a type")
    }

  private def stringConstFromQuotedString(s: String): StringConst =
    StringConst(s.tail.dropRight(1))

  private def parseNVars(t: Tokeniser, soFar: List[LexicalElem] = List()): MaybeLexicalElements =
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
