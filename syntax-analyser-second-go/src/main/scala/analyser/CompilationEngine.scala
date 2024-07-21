package analyser

import scala.annotation.tailrec
import util.chaining.scalaUtilChainingOps

object CompilationEngine {
  
  type MaybeLexicalElements = Either[String, List[LexicalElement]]
  
  def compileLet(tokeniser: Tokeniser): MaybeLexicalElements = {
    for {
      _ <- assertTokenEqualsAndAdvance(tokeniser, "let")
      variable <- getLexElementAsAndAdvance[LexicalIdentifier](tokeniser, LexicalElement.identifierFrom)
      _ <- assertTokenEqualsAndAdvance(tokeniser, "=")
      expression <- compileExpression(tokeniser)
      _ <- assertTokenEqualsAndAdvance(tokeniser, ";")
    } yield List(
      Keyword("let"), variable, LexicalSymbol('=')) ++ expression :+ LexicalSymbol(';')
  }
  
   def compileDo(t: Tokeniser): MaybeLexicalElements = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "do")
       variable <- getLexElementAsAndAdvance[LexicalIdentifier](t, LexicalElement.identifierFrom)
       _ <- assertTokenEqualsAndAdvance(t, ".")
       method <- getLexElementAsAndAdvance[LexicalIdentifier](t, LexicalElement.identifierFrom)
       _ <- assertTokenEqualsAndAdvance(t, "(")
       list <- getOptionalListOfVarsFollowedByClosingChar(t)
       _ <- assertTokenEqualsAndAdvance(t, ";")
     } yield List(Keyword("do"), variable, LexicalSymbol('.'), method) ++ wrapBrackets(list) :+ LexicalSymbol(';')
   }
   
   private def wrapBrackets(l: List[LexicalElement]): List[LexicalElement] = (LexicalSymbol('(') +: l) :+ LexicalSymbol(')') 
   private def wrapCurlyBrackets(l: List[LexicalElement]): List[LexicalElement] = (LexicalSymbol('{') +: l) :+ LexicalSymbol('}') 
   
   def compileVarDec(t: Tokeniser, classDec: Boolean = false): MaybeLexicalElements = {
     for {
       decType  <- getLexElementAsAndAdvance[Keyword](t, LexicalElement.varDecTypeFrom(_, classDec)) 
       varType  <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.keywordOrIndentifierFrom)
       varNames <- getVarDecList(t)
     } yield List(decType, varType) ++ varNames :+ LexicalSymbol(';')
   }
   
   def compileReturn(t: Tokeniser): MaybeLexicalElements = {
     for {
       _ <- assertTokenEqualsAndAdvance(t, "return")
       //TODO handle non empty returns
       _ <- assertTokenEqualsAndAdvance(t, ";")
     } yield List(Keyword("return"), LexicalSymbol(';'))
   }
   
   //TODO make compileStatements
   def compileStatement(t: Tokeniser): MaybeLexicalElements = {
     val r = t.currentToken match {
       case "let"      => compileLet(t)
       case "do"       => compileDo(t)
       case "if"       => compileIf(t)
       case "return"   => compileReturn(t)
       case otherToken => Left(s"uh oh $otherToken")
     }
     r
   }
   
   def compileParameterList(t: Tokeniser): MaybeLexicalElements = {
     for {
       _         <- assertTokenEqualsAndAdvance(t, "(")
       paramList <- getOptionalVarParamList(t)
     } yield LexicalSymbol('(') +: paramList :+ LexicalSymbol(')')
   }

   def compileSubroutineBody(t: Tokeniser): MaybeLexicalElements = {
     for {
       _          <- assertTokenEqualsAndAdvance(t, "{")
       varDecs    <- compileOptionalVarDecs(t)
       statements <- compileOptionalStatements(t)
       _          <- assertTokenEqualsAndAdvance(t, "}")
     } yield LexicalSymbol('{') +: List(varDecs, statements).flatten :+ LexicalSymbol('}')  
   }
   
   def compileSubroutine(t: Tokeniser): MaybeLexicalElements = {
     for {
       subroutineType <- getLexElementAsAndAdvance[Keyword](t, LexicalElement.subroutineTypeFrom)
       returnType <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.returnTypeFrom)
       subroutineName <- getLexElementAsAndAdvance[LexicalElement](t, LexicalElement.identifierFrom)
       params <- compileParameterList(t)
       body <- compileSubroutineBody(t)
     } yield List(subroutineType, returnType, subroutineName) ++ params ++ body
   }
   
   def compileClass(t: Tokeniser): MaybeLexicalElements = {
     for {
       _           <- assertTokenEqualsAndAdvance(t, "class")
       className   <- getLexElementAsAndAdvance[LexicalIdentifier](t, LexicalElement.identifierFrom)
       _           <- assertTokenEqualsAndAdvance(t, "{")
       classVars   <- compileOptionalVarDecs(t, true)
       subroutines <- compileOptionalSubroutines(t)
       _           <- assertTokenEqualsAndAdvance(t, "}")
     } yield List(Keyword("class"), className, LexicalSymbol('{')) ++ classVars ++ subroutines ++ List(LexicalSymbol('}')) 
   }
   
   def compileTerm(t: Tokeniser): MaybeLexicalElements = {
     val s = t.currentToken
     val lexElemOrError = TokenTypes.tokenType(s) match {
       case TokenTypes.IntConst => LexicalElement.intConstant(s)
       case TokenTypes.StringConst => LexicalElement.strConstant(s)
       case TokenTypes.Identifier => LexicalIdentifier(s)
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
           unaryOp <- getLexElementAsAndAdvance[LexicalSymbol](t, LexicalElement.operatorFrom(_, TokenTypes.UNARY_OPERATORS))
           term <- compileTerm(t)
         } yield unaryOp +: term
       case _ => ???
     }
     lexElemOrError match {
       case s: String => Left(s)
       case elem: LexicalElement => Right(List(elem)).tap(_ => t.advance())
       case maybeList: MaybeLexicalElements => maybeList
     }
   }
   
   @tailrec
   private def compileOptionalStatements(t: Tokeniser, current: List[LexicalElement] = List()): MaybeLexicalElements = {
     compileStatement(t) match {
       case Left(_) => Right(current)
       case Right(s) => compileOptionalStatements(t, current ++ s)
     }
   }
   
   private def compileOptional(t: Tokeniser, stop: Tokeniser => Boolean, compile: Tokeniser => MaybeLexicalElements) = {
     @tailrec
     def go(current: List[LexicalElement]): MaybeLexicalElements = {
       if (stop(t))
         Right(current)
       else  
         compile(t) match 
           case Left(err) => Left(err)
           case Right(tokens) => go(current ++ tokens)
     }
     go(List())
   }
   
   private def compileOptionalSubroutines(t: Tokeniser, current: List[LexicalElement] = List()): MaybeLexicalElements = {
     val stopFunction = (tokeniser: Tokeniser) => !TokenTypes.SUBROUTINE_TYPES.contains(tokeniser.currentToken)
     compileOptional(t, stopFunction, compileSubroutine)
   }
   
   private def compileOptionalVarDecs(t: Tokeniser, classDec: Boolean = false, current: List[LexicalElement] = List()): MaybeLexicalElements = {
     val stopFunction = (tokeniser: Tokeniser) => if (classDec) 
       !TokenTypes.CLASS_VAR_TYPES.contains(tokeniser.currentToken) 
     else 
       tokeniser.currentToken != "var"
     compileOptional(t, stopFunction, compileVarDec(_, classDec))
   }

   private def getOptionalVarParamList(t: Tokeniser, soFar: List[LexicalElement] = List()): MaybeLexicalElements = {
     t.currentToken match
       case ")" => Right(soFar).tap(_ => t.advance())
       case _ => getVarParamList(t, List())
   } 
   
   def compileIf(t: Tokeniser): MaybeLexicalElements = { //TODO add else
     for {
       _ <- assertTokenEqualsAndAdvance(t, "if")
       _ <- assertTokenEqualsAndAdvance(t, "(")
       expression <- compileExpression(t)
       _ <- assertTokenEqualsAndAdvance(t, ")")
       _ <- assertTokenEqualsAndAdvance(t, "{")
       letElems <- compileLet(t)
       _ <- assertTokenEqualsAndAdvance(t, "}")
     } yield (Keyword("let") +: wrapBrackets(expression)) ++ wrapCurlyBrackets(letElems)
   }

  def compileExpression(t: Tokeniser): Either[String, List[LexicalElement]] =
    for {
      term <- compileTerm(t)
      opTerm <- if (t.hasMoreTokens && Operator.isOperator(t.currentToken)) {
                  for {
                    operator <- getLexElementAsAndAdvance[LexicalSymbol](t, LexicalElement.operatorFrom(_, TokenTypes.OPERATORS))
                    nextTerm <- compileTerm(t)
                  } yield operator +: nextTerm
                } else
                    Right(List())
      } yield term ++ opTerm

  private def getOptionalListOfVarsFollowedByClosingChar(t: Tokeniser, varListSoFar: List[LexicalElement] = List()): MaybeLexicalElements =
    t.currentToken match
      case ")" => Right(varListSoFar).tap(_ => t.advance())
      case _ => getVarList(t, List())


  private def getVarParamList(t: Tokeniser, soFar: List[LexicalElement] = List()): MaybeLexicalElements =
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


  private def getVarDecList(t: Tokeniser, soFar: List[LexicalElement] = List()): MaybeLexicalElements =
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

  private def getVarList(t: Tokeniser, soFar: List[LexicalElement] = List()): MaybeLexicalElements =
    for {
      lexElem <- compileExpression(t)
      nextToken <- assertNextTokenEqualsOneOf(t, Set(")", ","))
      continue = nextToken == ","
      newVarList = soFar ++ lexElem
      r <- if (continue)
          getVarList(t, newVarList :+ LexicalSymbol(','))
        else
          Right(newVarList)
    } yield r

  private def assertNextTokenEqualsOneOf(t: Tokeniser, equals: Set[String]): Either[String, String] =
    t.currentToken match
      case s if equals.contains(s) => Right(s).tap(_ => t.advance())
      case otherString => Left(s"uh-oh, expected $otherString to equal ${equals.toList.mkString(" or ")}")

  private def getLexElementAsAndAdvance[T <: LexicalElement](t: Tokeniser, transformer: String => Either[String, T]): Either[String, T] =
    transformer(t.currentToken).tap(_ => t.advance())

  private def assertTokenEqualsAndAdvance(t: Tokeniser, equals: String): Either[String, Unit] =
    assertNextTokenEqualsOneOf(t, Set(equals)).map(_ => ())
}
