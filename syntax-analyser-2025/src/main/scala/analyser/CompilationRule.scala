package analyser

import analyser.CompilationEngine.compileExpressionList

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

type MaybeLexicalElements = Either[String, List[LexicalElem]]

sealed trait CompilationRule {
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

val openBracket = SymbolRule("(")
val openCurlyBracket = SymbolRule("{")
val closeBracket = SymbolRule(")")
val closeCurlyBracket = SymbolRule("}")
val semicolon = SymbolRule(";")

private def stringMatchingRule(t: Tokeniser, stringsToMatch: List[String], transformer: String => LexicalElem): MaybeLexicalElements =
  val s = t.currentToken
  if (stringsToMatch.contains(s))
    t.safeAdvance.map(_ => List(transformer(s)))
  else
    Left(s"expected one of $stringsToMatch, got ${t.currentToken}")

case class ObjectRule(rules: List[CompilationRule], enclosingElem: Option[String] = None) extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements =
    compileWithRules(t, rules, enclosingElem)
}

case class KeywordMatchingOneOfRule(stringsToMatch: List[String]) extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements =
    stringMatchingRule(t, stringsToMatch, Keyword.apply)
}

case class SymbolMatchingOneOfRule(symbolsToMatch: List[String]) extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements =
    stringMatchingRule(t, symbolsToMatch, (s: String) => Symbol(s.head))
}

case object VarRule extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements =
    val s = t.currentToken
    TokenTypes.tokenType(s) match
      case TokenTypes.Identifier => t.safeAdvance.flatMap(_ => Right(List(Identifier(s))))
      case otherTokenType => Left(s"expected token type identifier for string $s but got $otherTokenType")
}

case class CustomRule(rule: Tokeniser => MaybeLexicalElements) extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements = rule(t)
}

case object ExpressionListRule extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements = compileExpressionList(t).map(_._1)
}

case class TypeRule(includeVoid: Boolean = false) extends CompilationRule {
  override def compile(t: Tokeniser): MaybeLexicalElements =
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
}

case class OptionalElemRule(condition: String => Boolean, rule: Tokeniser => MaybeLexicalElements, advanceIfConditionMet: Boolean = true) extends CompilationRule {
  override def compile(t: Tokeniser) =
    if (condition(t.currentToken))
      if (advanceIfConditionMet)
        t.safeAdvance.flatMap(_ => rule(t))
      else rule(t)
    else
      Right(List())
}

case class ZeroOrMoreRule(startTokens: List[String], transformer: Tokeniser => MaybeLexicalElements, enclosingTag: Option[String] = None) extends CompilationRule {
  override def compile(t: Tokeniser) =
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
}

case class BranchingRule(ruleMap: Map[String, Tokeniser => MaybeLexicalElements]) extends CompilationRule {
  override def compile(t: Tokeniser) = {
    ruleMap.get(t.currentToken) match {
      case Some(transformer) => transformer(t)
      case None => Left(s"uh oh tried to perform branching rule for possibilities ${ruleMap.keys} with token ${t.currentToken}")
    }
  }
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

  val r = go(List(), rules)
  println(s"finished parsing elem with rules starting ${rules.head} got ${r}")
  r

private def encloseWithTags(tagname: String, elems: List[LexicalElem]) =
  StartElem(tagname) +: elems :+ EndElem(tagname)