package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CompilationEngineTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private def k(s: String) = Keyword(s)

  private def sym(c: Char) = Symbol(c)

  private def id(s: String) = Identifier(s)

  private def int(n: Int) = IntConst(n)

  private def str(s: String) = StringConst(s)

  object StatementsHelper {
    val doFooDotBarString = "do foo . bar ( ) ;"
    val letCountEqual5String = "let count = 5 ;"
    val returnCountString = "return count ;"
    val whileTrueDoFooDotBarString = s"while ( true ) { $doFooDotBarString }"
    val ifTrueDoFooDotBarString = s"if ( true ) { ${doFooDotBarString} }"
    private val doFooDotBarTokens = List(k("do"), id("foo"), sym('.'), id("bar"), sym('('), sym(')'), sym(';'))
    val statementsToTokens: Map[String, List[LexicalElem]] = Map(
      doFooDotBarString -> doFooDotBarTokens,
      letCountEqual5String -> List(k("let"), id("count"), sym('='), int(5), sym(';')),
      returnCountString -> List(k("return"), id("count"), sym(';')),
      whileTrueDoFooDotBarString -> (List(k("while")) ++ wrapBracket(List(k("true"))) ++ wrapCurly(doFooDotBarTokens)),
      ifTrueDoFooDotBarString -> (List(k("if"), Symbol('('), k("true"), Symbol(')')) ++ wrapCurly(doFooDotBarTokens))
    )

  }

  def wrapInStatement(elems: List[LexicalElem], statementType: String) =
    StartElem(statementType) +: (elems :+ EndElem(statementType))

  val validLetStatements = Table(("statement", "expected Tokens"),
    ("let count = count ;", List(k("let"), id("count"), sym('='), id("count"), sym(';'))),
    (StatementsHelper.letCountEqual5String, StatementsHelper.statementsToTokens(StatementsHelper.letCountEqual5String)),
    ("let count = 500 ;", List(k("let"), id("count"), sym('='), int(500), sym(';'))),
    ("let count = true ;", List(k("let"), id("count"), sym('='), k("true"), sym(';'))),
    ("let count = \"hi\" ;", List(k("let"), id("count"), sym('='), str("hi"), sym(';'))),
    ("let another_count = count ;", List(k("let"), id("another_count"), sym('='), id("count"), sym(';')))
  )

  //TODO more complex
  //other compile Lets
  //"let count = myArray[5] ;"
  //"let count = ( 4 + 5 ) * 10 ;"
  //"let count = ~ ~ true ;"
  //"let count = ~ true ;"
  //"let count = Foo.bar() ;"
  //"let count = Foo.bar(expressionList) ;" -expand with more examples
  //"let count = subroutineName('expressionList') ;" - expand with more examples
  //"let myArray[5] = 10 ;"
  //"let count = another_count * 5 ;"

  "compileLet" should "compile a valid let statement" in {
    forAll(validLetStatements) { case (statementString, expectedTokens) =>
      val tokeniser = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileLet(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid let statement" in {
    val invalidLets = Table(
      "invalid statement",
      "let count ;",
      "let 5 = 10 ;",
      "let foo = bar",
      "let foo * bar ; ",
      "let",
      "let ;",
      "let foo = ;",
      "let foo = ;"
    )

    forAll(invalidLets) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement ++ " rest of programme")
      CompilationEngine.compileLet(tokeniser).isLeft shouldBe true
    }
  }



  "compileDo" should "compile a do statement" in {
    import StatementsHelper.statementsToTokens
    import StatementsHelper.doFooDotBarString

    val validDoStatements = Table(
      ("statement", "expectedTokens"),
      ("do subroutineName ( ) ;", List(k("do"), id("subroutineName"), sym('('), sym(')'), sym(';'))),
      (doFooDotBarString, statementsToTokens(doFooDotBarString)),
      ("do subroutineName ( 5 ) ;", List(k("do"), id("subroutineName"), sym('('), int(5), sym(')'), sym(';'))),
      ("do subroutineName ( 5 , 4 ) ;", List(k("do"), id("subroutineName"), sym('('), int(5), sym(','), int(4), sym(')'), sym(';'))),
      ("do foo . bar ( \"hi\" , myVar ) ;", List(k("do"), id("foo"), sym('.'), id("bar"), sym('('), str("hi"), sym(','), id("myVar"), sym(')'), sym(';')))
    )
    //TODO more complex expression lists (although this should be handled by implementing expressions

    forAll(validDoStatements) { case (statement, expectedTokens) =>
      val tokeniser = testTokeniser(statement + " rest of programme")
      CompilationEngine.compileDo(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid do statements" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "do ;",
      "do foo ;",
      "do foo . ;",
      "do 5 . bar ( ) ;",
      "do foo . bar ( ;",
      "do foo . bar ( )",
      "do this . that ( ) ;" //this is not an identifier
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileDo(tokeniser).isLeft shouldBe true
    }
  }

  "compile while" should "compile a valid while statement" in {
    import StatementsHelper.*
    val doFooDotBarTokens = statementsToTokens(doFooDotBarString)
    val validWhileStatements = Table(
      ("validStatements", "expectedTokens"),
      (whileTrueDoFooDotBarString, statementsToTokens(whileTrueDoFooDotBarString)),
      (s"while ( true ) { }", (k("while") +: wrapBracket(List(k("true")))) ++ wrapCurly(List())),
      (s"while ( true ) { $doFooDotBarString $doFooDotBarString }", (k("while") +: wrapBracket(List(k("true")))) ++ wrapCurly(doFooDotBarTokens ++ doFooDotBarTokens)),
      //TODO more whiles - not super necessary though
    )

    forAll(validWhileStatements) { case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement + " rest of programme")
      CompilationEngine.compileWhile(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid while statement" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "while ;",
      "while true { do foo . bar () ; }",
      "while ( true ) do foo . bar () ; }"
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileWhile(tokeniser).isLeft shouldBe true
    }
  }

  "compile if" should "compile a valid if statement" in {
    import StatementsHelper.*

    val validIfStatements = Table(
      ("validStatement", "expectedTokens"),
      (ifTrueDoFooDotBarString, statementsToTokens(ifTrueDoFooDotBarString)),
      (s"if ( true ) { ${doFooDotBarString} } else { ${doFooDotBarString} }", List(k("if"), Symbol('('), k("true"), Symbol(')')) ++ wrapCurly(statementsToTokens(doFooDotBarString)) ++ List(k("else")) ++ wrapCurly(statementsToTokens(doFooDotBarString))),
      //TODO more - not super necessary
    )

    forAll(validIfStatements) { case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement + " rest of programme")
      CompilationEngine.compileIf(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid if statement" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "if ;",
      "if true { do foo . bar () ; }",
      "if ( true ) do foo . bar () ; }"
      //TODO more
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileIf(tokeniser).isLeft shouldBe true
    }
  }

  "compile return" should "compile a valid return statement" in {
    val validReturns = Table(
      ("statement", "expectedTokens"),
      ("return ;", List(k("return"), sym(';'))),
      ("return 5 ;", List(k("return"), int(5), sym(';')))
      //TODO more complex expressions (to be handled once expressions fully implemented)
    )

    forAll(validReturns) { case (statement, expectedTokens) =>
      val tokeniser = testTokeniser(statement + " rest of programme")
      CompilationEngine.compileReturn(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid return statement" in {
    val invalidReturns = Table(
      "invalidStatement",
      "retrn ;",
      "return",
      "return class ;",
      "return 4 ~ 5 ;"
      //TODO more complex expressions (to be handled once expressions fully implemented)
    )

    forAll(invalidReturns) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileReturn(tokeniser).isLeft shouldBe true
    }
  }

  val validVarDecs = (varDecTypeString: String) => Table(
    ("statementString", "expectedTokens"),
    (s"$varDecTypeString boolean myBool ;", List(k(varDecTypeString), k("boolean"), id("myBool"), sym(';'))),
    (s"$varDecTypeString boolean myBool , mySecondBool ;", List(k(varDecTypeString), k("boolean"), id("myBool"), sym(','), id("mySecondBool"), sym(';'))),
    (s"$varDecTypeString boolean myBool , mySecondBool , myThirdBool ;", List(k(varDecTypeString), k("boolean"), id("myBool"), sym(','), id("mySecondBool"), sym(','), id("myThirdBool"), sym(';'))),
    (s"$varDecTypeString int myNumber ;", List(k(varDecTypeString), k("int"), id("myNumber"), sym(';'))),
    (s"$varDecTypeString char myChar ;", List(k(varDecTypeString), k("char"), id("myChar"), sym(';'))),
    (s"$varDecTypeString myClass myClassInstance ;", List(k(varDecTypeString), id("myClass"), id("myClassInstance"), sym(';')))
  )

  val invalidVarDecs = (varDecString: String) => Table(
    "invalidstatementString",
    s"$varDecString boolean myBool", //no closing ;
    s"$varDecString boolean ;", //no vars ;
    s"$varDecString boolean myBool , mySecondBool ", //no closing ;
    s"$varDecString boolean myBool , mySecondBool , ;", //dangling comma
    s"$varDecString int 5 ;", //not a var name
    s"$varDecString this myChar ;", //not a valid type (reserved keyword)
    "staticField boolean notAProperFieldName ; ", //neither $varDecString nor field
    s"$varDecString 5 int ;", //5 not a valid identifier
    s"$varDecString", //variants of dangling statements
    s"$varDecString boolean", //variants of dangling statements
    s"$varDecString boolean myBool + myOtherBool ;" //invalid joining char
  )

  "compileClassVarDec" should "compile a valid class var for a static var" in {
    forAll(validVarDecs("static")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "compile a valid class var for a field var" in {
    forAll(validVarDecs("field")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid class var dec" in {
    forAll(invalidVarDecs("static") ++ invalidVarDecs("field")) { invalidString =>
      val t = testTokeniser(invalidString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t).isLeft shouldBe true
    }
  }

  "compileVarDec" should "compile a valid var dec" in {
    forAll(validVarDecs("var")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid var decs" in {
    forAll(invalidVarDecs("var")) { case invalidVarDecString =>
      val t = testTokeniser(invalidVarDecString ++ " rest of programme")
      CompilationEngine.compileVarDec(t).isLeft shouldBe true
    }
  }

  "compileParameterList" should "compile a valid parameter list" in {
    val validParameterLists = Table(
      ("validStatement", "expectedTokens"),
      (")", List()),
      ("boolean myBool", List(k("boolean"), id("myBool"))),
      ("boolean myBool , int myInt , className myClass , char myChar", List(k("boolean"), id("myBool"), sym(','), k("int"), id("myInt"), sym(','), id("className"), id("myClass"), sym(','), k("char"), id("myChar")))
    )

    forAll(validParameterLists) { case (statementString, expectedTokens) =>
      val closingChar = ")"
      val t = testTokeniser(statementString ++ s" $closingChar")
      CompilationEngine.compileParameterList(t, closingChar) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid parameter lists" in {
    val invalidParameterList = Table(
      "invalidParam",
      "this foo",
      "foo , bar",
      "boolean foo , this bar",
      "boolean myBool anotherBool"
    )
    forAll(invalidParameterList) { case invalidParameterListString =>
      val closingChar = ")"
      val t = testTokeniser(invalidParameterListString ++ s"$closingChar)")
      CompilationEngine.compileParameterList(t, closingChar).isLeft shouldBe true
    }
  }

  "compileSubroutineBody" should "compile a valid subroutine body" in {
    val myBoolDeclaration = "var boolean myBool ;"
    val myBoolDeclarationTokens = List(k("var"), k("boolean"), id("myBool"), sym(';'))
    val charDeclarations = "var char charA , charB ;"
    val charDeclarationTokens =  List(k("var"), k("char"), id("charA"), sym(','), id("charB"), sym(';'))
    val letFooEqualStatement = "let foo = myBool ;"
    val letFooEqualTokens = List(k("let"), id("foo"), sym('='), id("myBool"), sym(';'))
    val returnStatement = "return foo ;"
    val returnTokens = List(k("return"), id("foo"), sym(';'))
    val validSubroutineBodies = Table(
      ("validStatement", "expectedTokens"),
      (
        s"{ $myBoolDeclaration $letFooEqualStatement }",
        wrapCurly(myBoolDeclarationTokens ++ letFooEqualTokens)
      ),
      (
        s"{ $myBoolDeclaration $charDeclarations $letFooEqualStatement }",
        wrapCurly(myBoolDeclarationTokens ++ charDeclarationTokens ++ letFooEqualTokens)
      ),
      (
        s"{ $letFooEqualStatement $returnStatement }",
        wrapCurly(letFooEqualTokens ++ returnTokens)
      )
    )

    forAll(validSubroutineBodies){ case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement ++ " rest of programme")
      CompilationEngine.compileSubroutineBody(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  object SubroutineHelper {
    val functionDec = "function int returnV1 ( int v1 , int v2 ) { return v1 ; }"
    val functionDecTokens = List(k("function"), k("int"), id("returnV1"), sym('('), k("int"), id("v1"), sym(','), k("int"), id("v2"), sym(')'), sym('{'), k("return"), id("v1"), sym(';'), sym('}'))
    val methodDec = "method void returnTrue ( String string , int i ) { return true ; } "
    val methodDecTokens = List(k("method"), k("void"), id("returnTrue"), sym('('), id("String"), id("string"), sym(','), k("int"), id("i"), sym(')'), sym('{'), k("return"), k("true"), sym(';'), sym('}'))
    val constructorDec = "constructor myClass create ( String string ) { return myClassInstance ; }"
    val constructorTokens = List(k("constructor"), id("myClass"), id("create"), sym('('), id("String"), id("string"), sym(')'), sym('{'), k("return"), id("myClassInstance"), sym(';'), sym('}'))
  }

  "compileSubroutine" should "compile a valid subroutine" in {
    import SubroutineHelper.*
    val validSubroutines = Table(
      ("validString", "expectedTokens"),
      (functionDec, functionDecTokens),
      (methodDec, methodDecTokens),
      (constructorDec, constructorTokens)
    )

    forAll(validSubroutines) { case (validString, expectedTokens) =>
      val tokeniser = testTokeniser(validString ++ " rest of input")
      CompilationEngine.compileSubroutine(tokeniser) shouldBe Right(expectedTokens)
    }


    //TODO more examples
  }

  //TODO
  "compile class" should "compile a valid class" in {
    val classVarDecs = "static int foo ;"
    val classVarDecTokens = List(k("static"), k("int"), id("foo"), sym(';'))
    val validClassString =
      s"class myClass { $classVarDecs ${SubroutineHelper.functionDec} }"

    val tokeniser = testTokeniser(validClassString)
    CompilationEngine.compileClass(tokeniser) shouldBe Right(List(k("class"), id("myClass")) ++ wrapCurly(classVarDecTokens ++ SubroutineHelper.functionDecTokens))
  }


  private def wrapCurly(s: String) = if (s.nonEmpty) "{ " + s + " }" else "{ }"

  private def wrapCurly(l: List[LexicalElem]) = (sym('{') +: l) :+ sym('}')

  private def wrapCurly(elems: LexicalElem*): List[LexicalElem] = (sym('{') +: elems.toList) :+ sym('}')

  private def wrapBracket(s: String) = if (s.nonEmpty) "( " + s + " )" else "( )"

  private def wrapBracket(l: List[LexicalElem]) = (sym('(') +: l) :+ sym(')')

  private def wrapBracket(elems: LexicalElem*): List[LexicalElem] = (sym('(') +: elems.toList) :+ sym(')')

  class FakeTokeniser(var tokens: List[String]) extends Tokeniser {
    override def advance(): Unit = tokens = tokens.tail

    def allTokens: Seq[String] = tokens.tail

    override def currentToken: String = tokens.head

    override def hasMoreTokens: Boolean = tokens.tail.nonEmpty
  }

  def testTokeniser(l: List[String]) = new FakeTokeniser(l)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
