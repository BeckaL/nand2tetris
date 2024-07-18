package analyser

import scala.collection.mutable.ArrayBuffer
import util.chaining.scalaUtilChainingOps

object CompilationEngine {
  
  def compileLetGetLexicalElements(tokeniser: Tokeniser): Either[String, List[LexicalElement]] = {
    val lexicalElements = ArrayBuffer[LexicalElement]()
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      _ = lexicalElements.addOne(Keyword("let"))
      variable <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ = lexicalElements.addOne(variable.toLexElem)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      _ = lexicalElements.addOne(LexicalSymbol('='))
      expression <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ = lexicalElements.addOne(expression.toLexElem)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
      _ = lexicalElements.addOne(LexicalSymbol(';'))
    } yield lexicalElements.toList
  }
  
  def compileLet(tokeniser: Tokeniser): Either[String, LetStatement] =
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      variable <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      expression <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
    } yield LetStatement(variable, expression)

   def compileDoGetLexicalElements(t: Tokeniser): Either[String, List[LexicalElement]] = {
     val lexicalElements = ArrayBuffer[LexicalElement]()

     for {
       _ <- assertTokenEqualsAndAdvance(t, "do")
       _ = lexicalElements.addOne(Keyword("do"))
       variable <- getTokenAsAndAdvance[Term](t, Term.from)
       _ = lexicalElements.addOne(variable.toLexElem)
       _ <- assertTokenEqualsAndAdvance(t, ".")
       _ = lexicalElements.addOne(LexicalSymbol('.'))
       method <- getTokenAsAndAdvance[Term](t, Term.from)
       _ = lexicalElements.addOne(method.toLexElem)
       _ <- assertTokenEqualsAndAdvance(t, "(")
       _ = lexicalElements.addOne(LexicalSymbol('('))
       list <- getOptionalListOfVarsFollowedByClosingChar(t)
       _ = lexicalElements.addOne(list.head.toLexElem)
       _ = lexicalElements.addOne(LexicalSymbol(')'))
       _ <- assertTokenEqualsAndAdvance(t, ";")
       _ = lexicalElements.addOne(LexicalSymbol(';'))
     } yield lexicalElements.toList
   }

  def compileDo(t: Tokeniser): Either[String, DoStatement] =
    for {
      _ <- assertTokenEqualsAndAdvance(t, "do")
      variable <- getTokenAsAndAdvance[Term](t, Term.from)
      _ <- assertTokenEqualsAndAdvance(t, ".")
      method <- getTokenAsAndAdvance[Term](t, Term.from)
      _ <- assertTokenEqualsAndAdvance(t, "(")
      list <- getOptionalListOfVarsFollowedByClosingChar(t)
      _ <- assertTokenEqualsAndAdvance(t, ";")
    } yield DoStatement(variable, method, list)

  def compileExpression(t: Tokeniser): Either[String, Expression] =
    for {
      term <- getTokenAsAndAdvance[Term](t, Term.from)
      opTerm <- if (t.hasMoreTokens && Operator.isOperator(t.currentToken)) {
                  for {
                    operator <- getTokenAsAndAdvance[Operator](t, Operator.from)
                    nextTerm <- getTokenAsAndAdvance[Term](t, Term.from)
                  } yield Some(operator, nextTerm)
                } else
                    Right(None)
      } yield Expression(term, opTerm)

  private def getOptionalListOfVarsFollowedByClosingChar(t: Tokeniser, varListSoFar: List[Term] = List()): Either[String, List[Term]] =
    t.currentToken match
      case ")" => Right(varListSoFar).tap(_ => t.advance())
      case _ => getVarList(t, List())

  private def getVarList(t: Tokeniser, soFar: List[Term] = List()): Either[String, List[Term]] =
    for {
      varName <- getTokenAsAndAdvance[Term](t, Term.from)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar :+ varName
      r <- if (continue)
          getVarList(t, newVarList)
        else
          Right(newVarList)
    } yield r

  private def assertNextTokenEqualsOneOf(t: Tokeniser, equals: Set[String]): Either[String, String] =
    t.currentToken match
      case s if equals.contains(s) => Right(s).tap(_ => t.advance())
      case otherString => Left(s"uh-oh, expected $otherString to equal ${equals.toList.mkString(" or ")}")

  private def getTokenAsAndAdvance[T <: Token](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def assertTokenEqualsAndAdvance(t: Tokeniser, equals: String): Either[String, Unit] =
    assertNextTokenEqualsOneOf(t, Set(equals)).map(_ => ())
}
