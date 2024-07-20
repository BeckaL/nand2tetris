package analyser

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import util.chaining.scalaUtilChainingOps

object CompilationEngine {
  
  def compileLet(tokeniser: Tokeniser): Either[String, List[LexicalElement]] = {
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      variable <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      expression <- getTokenAsAndAdvance[Term](tokeniser, Term.from)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
    } yield List(
      Keyword("let"), variable.toLexElem, LexicalSymbol('='), expression.toLexElem, LexicalSymbol(';')
    )
  }
  
   def compileDo(t: Tokeniser): Either[String, List[LexicalElement]] = {
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
       _ = lexicalElements.addAll(list)
       _ = lexicalElements.addOne(LexicalSymbol(')'))
       _ <- assertTokenEqualsAndAdvance(t, ";")
       _ = lexicalElements.addOne(LexicalSymbol(';'))
     } yield lexicalElements.toList
   }
   
   def compileVarDec(t: Tokeniser): Either[String, List[LexicalElement]] = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "var")
       varType <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.keywordOrIndentifierFrom)
       varNames <- getVarDecList(t)
     } yield List(Keyword("var"), varType) ++ varNames :+ LexicalSymbol(';')
   }
   
   def compileReturn(t: Tokeniser): Either[String, List[LexicalElement]] = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "return")
       //TODO handle non empty returns
       _ <- assertTokenEqualsAndAdvance(t, ";")
     } yield List(Keyword("return"), LexicalSymbol(';'))
   }
   
   //TODO make compileStatements
   def compileStatement(t: Tokeniser): Either[String, List[LexicalElement]] = {
     val r = t.currentToken match {
       case "let" => compileLet(t)
       case "do" => compileDo(t)
       case "if" => compileIf(t)
       case "return" => compileReturn(t)
       case otherToken => Left(s"uh oh $otherToken")
     }
     r
   }
   
   def compileParameterList(t: Tokeniser): Either[String, List[LexicalElement]] = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "(")
       paramList <- getOptionalVarParamList(t)
     } yield LexicalSymbol('(') +: paramList :+ LexicalSymbol(')')
   }

   def compileSubroutineBody(t: Tokeniser): Either[String, List[LexicalElement]] = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "{")
       varDecs <- compileOptionalVarDecs(t)
       statements <- compileOptionalStatements(t)
       _ <- assertTokenEqualsAndAdvance(t, "}")
     } yield LexicalSymbol('{') +: List(varDecs, statements).flatten :+ LexicalSymbol('}')  
   }
   
   def compileSubroutine(t: Tokeniser): Either[String, List[LexicalElement]] = {
     for {
       subroutineType <- getLexElementAsAndAdvance[Keyword](t, LexicalElement.subroutineTypeFrom)
       returnType <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.returnTypeFrom)
       subroutineName <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.identifierFrom)
       params <- compileParameterList(t)
       body <- compileSubroutineBody(t)
     } yield List(subroutineType, returnType, subroutineName) ++ params ++ body
   }
   
   @tailrec
   private def compileOptionalStatements(t: Tokeniser, current: List[LexicalElement] = List()): Either[String, List[LexicalElement]] = {
     compileStatement(t) match {
       case Left(_) => Right(current)
       case Right(s) => compileOptionalStatements(t, current ++ s)
     }
   }
   
   @tailrec
   private def compileOptionalVarDecs(t: Tokeniser, current: List[LexicalElement] = List()): Either[String, List[LexicalElement]] = {
     if (t.currentToken != "var") 
       Right(current)
     else 
       compileVarDec(t) match
         case Left(s) => Left(s)
         case Right(tokens) => compileOptionalVarDecs(t, current ++ tokens)
   }

   private def getOptionalVarParamList(t: Tokeniser, soFar: List[LexicalElement] = List()): Either[String, List[LexicalElement]] = {
     t.currentToken match
       case ")" => Right(soFar).tap(_ => t.advance())
       case _ => getVarParamList(t, List())
   } 
   
   def compileIf(t: Tokeniser): Either[String, List[LexicalElement]] = {
     val lexicalElements = ArrayBuffer[LexicalElement]()

     for {
       _ <- assertTokenEqualsAndAdvance(t, "if")
       _ = lexicalElements.addOne(Keyword("if"))
       _ <- assertTokenEqualsAndAdvance(t, "(")
       _ = lexicalElements.addOne(LexicalSymbol('('))
       variable <- getTokenAsAndAdvance[Term](t, Term.from)
       _ = lexicalElements.addOne(variable.toLexElem)
       operator <- getTokenAsAndAdvance[Operator](t, Operator.from)
       _ = lexicalElements.addOne(operator.toLexElem)
       evaluatedTo <- getTokenAsAndAdvance[Term](t, Term.from)
       _ = lexicalElements.addOne(evaluatedTo.toLexElem)
       _ <- assertTokenEqualsAndAdvance(t, ")")
       _ = lexicalElements.addOne(LexicalSymbol(')'))
       _ <- assertTokenEqualsAndAdvance(t, "{")
       _ = lexicalElements.addOne(LexicalSymbol('{'))
       letElems <- compileLet(t)
       _ = lexicalElements.addAll(letElems)
       _ <- assertTokenEqualsAndAdvance(t, "}")
       _ = lexicalElements.addOne(LexicalSymbol('}'))
     } yield lexicalElements.toList
     
   }

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

  private def getOptionalListOfVarsFollowedByClosingChar(t: Tokeniser, varListSoFar: List[LexicalElement] = List()): Either[String, List[LexicalElement]] =
    t.currentToken match
      case ")" => Right(varListSoFar).tap(_ => t.advance())
      case _ => getVarList(t, List())


  private def getVarParamList(t: Tokeniser, soFar: List[LexicalElement] = List()): Either[String, List[LexicalElement]] =
    for {
      varType <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.keywordOrIndentifierFrom) //TODO this can return an invalid type
      identifier <- getLexElementAsAndAdvance[LexicalIdentifier](t, LexicalElement.identifierFrom)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar ++ List(varType, identifier)
      r <- if (continue)
        getVarParamList(t, newVarList :+ LexicalSymbol(','))
      else
        Right(newVarList)
    } yield r


  private def getVarDecList(t: Tokeniser, soFar: List[LexicalElement] = List()): Either[String, List[LexicalElement]] =
    for {
      lexElem <- getLexElementAsAndAdvance[LexicalIdentifier](t, LexicalElement.identifierFrom)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(";", ","))
      continue = nextToken == ","
      newVarList = soFar :+ lexElem
      r <- if (continue)
        getVarDecList(t, newVarList :+ LexicalSymbol(','))
      else
        Right(newVarList)
    } yield r

  private def getVarList(t: Tokeniser, soFar: List[LexicalElement] = List()): Either[String, List[LexicalElement]] =
    for {
      lexElem <- getTokenAsAndAdvance[Term](t, Term.from).map(_.toLexElem)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar :+ lexElem
      r <- if (continue)
          getVarList(t, newVarList :+ LexicalSymbol(','))
        else
          Right(newVarList)
    } yield r

  private def assertNextTokenEqualsOneOf(t: Tokeniser, equals: Set[String]): Either[String, String] =
    t.currentToken match
      case s if equals.contains(s) => Right(s).tap(_ => t.advance())
      case otherString => Left(s"uh-oh, expected $otherString to equal ${equals.toList.mkString(" or ")}")

  private def getTokenAsAndAdvance[T <: Token](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def getLexElementAsAndAdvance[T <: LexicalElement](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def assertTokenEqualsAndAdvance(t: Tokeniser, equals: String): Either[String, Unit] =
    assertNextTokenEqualsOneOf(t, Set(equals)).map(_ => ())
}
