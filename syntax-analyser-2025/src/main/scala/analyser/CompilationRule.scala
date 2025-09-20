package analyser

import analyser.CompilationEngine.compileExpressionList

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

type MaybeLexicalElements = Either[String, List[LexicalElem]]

def keywordRule(stringToMatch: String)(t: Tokeniser): MaybeLexicalElements =
  stringMatchingRule(t, List(stringToMatch), Keyword.apply)

def symbolRule(stringToMatch: String)(t: Tokeniser) =
  stringMatchingRule(t, List(stringToMatch), (s: String) => Symbol(s.head))

val openBracket = symbolRule("(")
val openCurlyBracket = symbolRule("{")
val closeBracket = symbolRule(")")
val closeCurlyBracket = symbolRule("}")
val semicolon = symbolRule(";")

private def stringMatchingRule(t: Tokeniser, stringsToMatch: List[String], transformer: String => LexicalElem): MaybeLexicalElements =
  val s = t.currentToken
  if (stringsToMatch.contains(s))
    t.safeAdvance.map(_ => List(transformer(s)))
  else
    Left(s"expected one of $stringsToMatch, got ${t.currentToken}")

def keywordMatchingOneOfRule(stringsToMatch: List[String])(t: Tokeniser) =
  stringMatchingRule(t, stringsToMatch, Keyword.apply)
  
def symbolMatchingOneOfRule(symbolsToMatch: List[String])(t: Tokeniser) =
  stringMatchingRule(t, symbolsToMatch, (s: String) => Symbol(s.head))

val varRule = (t: Tokeniser) => {
  val s = t.currentToken
  TokenTypes.tokenType(s) match
    case TokenTypes.Identifier => t.safeAdvance.flatMap(_ => Right(List(Identifier(s))))
    case otherTokenType => Left(s"expected token type identifier for string $s but got $otherTokenType")
}

val expressionListRule = (t: Tokeniser) => {
  compileExpressionList(t).map(_._1) //TODO
}

def typeRule(includeVoid: Boolean = false)(t: Tokeniser) =
  val currentToken = t.currentToken
  TokenTypes.tokenType(currentToken) match {
    case TokenTypes.Keyword =>
      val variableTypes = List("boolean", "char", "int")
      val typesToCheck = if (includeVoid) "void" +: variableTypes else variableTypes
      if (typesToCheck.contains(currentToken))
        t.safeAdvance.flatMap(_ => Right(List(Keyword(currentToken))))
      else
        Left(s"keyword $currentToken cannot be used as a type, valid keyword types are boolean char or int")
    case TokenTypes.Identifier =>
      Right(List(Identifier(currentToken))).tap(_ => t.advance())
    case other =>
      Left(s"$currentToken cannot be used as a type")
  }

def optionalElemRule(condition: String => Boolean, rules: List[Tokeniser => MaybeLexicalElements], advanceIfConditionMet: Boolean = true, enclosingElem: Option[String] = None)(t: Tokeniser) =
  if (condition(t.currentToken))
    if (advanceIfConditionMet)
      t.safeAdvance.flatMap(_ => compileWithRules(t, rules, enclosingElem))
    else compileWithRules(t, rules, enclosingElem)
  else
    Right(List())

def zeroOrMoreRule(startTokens: List[String], transformer: Tokeniser => MaybeLexicalElements, enclosingTag: Option[String] = None)(t: Tokeniser) =
  @tailrec
  def go(soFar: List[LexicalElem]): MaybeLexicalElements =
    if (!startTokens.contains(t.currentToken)) {
      enclosingTag match
        case Some(tag) => Right(encloseWithTags(tag, soFar))
        case None => Right(soFar)
    } else {
      val result = transformer(t)
      result match
        case Left(err) => Left(err)
        case Right(newElems) => go(soFar ++ newElems)
    }

  go(List())
  
def branchingRule(ruleMap: Map[String, RuleTransformer])(t: Tokeniser) =
  ruleMap.get(t.currentToken) match {
    case Some(transformer) => transformer(t)
    case None => Left(s"uh oh tried to perform branching rule for possibilities ${ruleMap.keys} with token ${t.currentToken}")
  }
  
type RuleTransformer = Tokeniser => MaybeLexicalElements 

def compileWithRules(t: Tokeniser, rules: List[RuleTransformer], enclosingElem: Option[String]): MaybeLexicalElements =
  @tailrec
  def go(soFar: List[LexicalElem], remainingRules: List[RuleTransformer]): MaybeLexicalElements =
    remainingRules match
      case Nil =>
        enclosingElem match {
          case Some(elem) => Right(encloseWithTags(elem, soFar))
          case None => Right(soFar)
        }
      case firstRule :: otherRules =>
        firstRule(t) match
          case Left(err) => Left(err)
          case Right(newElems) => go(soFar ++ newElems, otherRules)

  go(List(), rules)

private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
  StartElem(tagname) +: elems :+ EndElem(tagname)