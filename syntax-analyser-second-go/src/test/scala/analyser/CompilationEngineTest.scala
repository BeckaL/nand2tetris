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

  "compileLet" should "compile a valid let statement" in {
    val tokeniser = testTokeniser("let count = count ;")
    CompilationEngine.compileLet(tokeniser) shouldBe Right(
      List(k("let"), id("count"), sym('='), id("count"), sym(';')))
  }

  it should "compile a valid let statement with an integer constant" in {
    val tokeniser = testTokeniser("let count = 100 ;")
    CompilationEngine.compileLet(tokeniser) shouldBe Right(
      List(k("let"), id("count"), sym('='), int(100), sym(';')))
  }

  it should "throw an error on an invalid let statement" in {
    val tokeniser = testTokeniser("let count * count")
    CompilationEngine.compileLet(tokeniser) shouldBe Left("uh-oh, expected * to equal =")
  }

  it should "compile a do statement with a single expression and no params" in {
    val tokeniser = testTokeniser("do square . dispose ( ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(
      List(k("do"), id("square"), sym('.'), id("dispose"), sym('('), sym(')'), sym(';')))
  }

  it should "compile a do statement with a single param" in {
    val tokeniser = testTokeniser("do Memory . deAlloc ( square ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(
      List(k("do"), id("Memory"), sym('.'), id("deAlloc"), sym('('), id("square"), sym(')'), sym(';')))
  }

  it should "compile a do statement with multiple params" in {
    val tokeniser = testTokeniser("do Foo . bar ( 100 , \"b\" , c ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(
      List(k("do"), id("Foo"), sym('.'), id("bar"), sym('('), int(100), sym(','), str("b"), sym(','), id("c"), sym(')'), sym(';')))
  }

  "compileVarDec" should "compile a varDeclaration" in {
    val tokeniser = testTokeniser("var int average ;")
    val expectedTokens = List(k("var"), k("int"), id("average"), sym(';'))

    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(expectedTokens)
  }

  it should "compile a varDeclaration with multiple vars" in {
    val tokeniser = testTokeniser("var int average , sum , count ;")
    val expectedTokens = List(k("var"), k("int"), id("average"), sym(','), id("sum"), sym(','), id("count"), sym(';'))

    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(expectedTokens)
  }

  it should "compile a class var declaration" in {
    val tokeniser = testTokeniser("static int average , sum , count ;")
    val expectedTokens = List(k("static"), k("int"), id("average"), sym(','), id("sum"), sym(','), id("count"), sym(';'))

    CompilationEngine.compileVarDec(tokeniser, true) shouldBe Right(expectedTokens)
  }

  it should "not compile an invalid class var declaration" in {
    val tokeniser = testTokeniser("void int average , sum , count ;")
    val expectedErrorMessage = "keyword void cannot be used as a var dec type"

    CompilationEngine.compileVarDec(tokeniser, true) shouldBe Left(expectedErrorMessage)
  }

  object CompileIfHelper {
    val ifTerm: String = "a < ( 100 - 10 )"
    val expectedIfTerm: List[LexicalElem] = wrapBracket(List(id("a"), sym('<')) ++ wrapBracket(int(100), sym('-'), int(10)))

    val letStatement = "let b = \"hi\" ;"
    val expectedLetStatement: List[LexicalElem] = List(k("let"), id("b"), sym('='), str("hi"), sym(';'))

    val doStatement = "do Output . print ( b ) ;"
    val expectedDoStatement = List(k("do"), id("Output"), sym('.'), id("print")) ++ wrapBracket(id("b")) :+ sym(';')

    val elseStatement = "else { " + doStatement + " }"
    val expectedElseStatement = k("else") +: wrapCurly(expectedDoStatement)
  }

  "compileIf" should "compile a valid if" in {
    import CompileIfHelper.*

    val data = Table(
      ("statements", "elseStatement", "expectedStatements"),
      (letStatement, None, wrapCurly(expectedLetStatement)),
      (doStatement, None, wrapCurly(expectedDoStatement)),
      (letStatement + " " + doStatement, None, wrapCurly(expectedLetStatement ++ expectedDoStatement)),
      (letStatement, Some(elseStatement), wrapCurly(expectedLetStatement) ++ expectedElseStatement)
    )

    forAll(data){ case (statements, maybeElseStatement, expectedStatements) =>
      val input = s"if ( $ifTerm ) { $statements }" ++ maybeElseStatement.map(" " + _).getOrElse("")
      val expected = (k("if") +: expectedIfTerm) ++ expectedStatements

      CompilationEngine.compileIf(testTokeniser(input)) shouldBe Right(expected)
    }
  }

  "compile return" should "compile an empty return statement" in {
    val tokeniser = testTokeniser("return ;")
    CompilationEngine.compileReturn(tokeniser) shouldBe Right(
      List(k("return"), sym(';'))
    )
  }

  it should "return an error for an invalid return" in {
    val tokeniser = testTokeniser("return + ;")
    CompilationEngine.compileReturn(tokeniser) shouldBe Left("uh-oh, expected + to equal ;")
  }

  "compileParameterList" should "compile an empty parameter list" in {
    val tokeniser = testTokeniser("( )")
    CompilationEngine.compileParameterList(tokeniser) shouldBe Right(wrapBracket(List()))
  }

  it should "compile a single parameter" in {
    val tokeniser = testTokeniser("( int myVar )")
    CompilationEngine.compileParameterList(tokeniser) shouldBe Right(
      wrapBracket(List(k("int"), id("myVar")))
    )
  }

  it should "compile multiple parameters" in {
    val tokeniser = testTokeniser("( int myVar , MyClass instanceOfMyClass )")
    CompilationEngine.compileParameterList(tokeniser) shouldBe Right(
      wrapBracket(List(k("int"), id("myVar"), sym(','), id("MyClass"), id("instanceOfMyClass")))
    )
  }

  it should "fail on a malformed param list" in {
    val tokeniser = testTokeniser("( int , myVar )")
    CompilationEngine.compileParameterList(tokeniser) shouldBe Left("Uh-oh, tried to parse identifier from ,")
  }

  object CompileSubroutineBodyHelper {
    val singleVarDec = "var MyClass blah ;"
    val singleVarDecTokens: List[LexicalElem] =
      List(k("var"), id("MyClass"), id("blah"), sym(';'));

    val multipleVarDecs = "var int count , sum ; var char myChar ;"
    val multipleVarDecsTokens: List[LexicalElem] =
      List(k("var"), k("int"), id("count"), sym(','), id("sum"), sym(';'), k("var"), k("char"), id("myChar"), sym(';'))

    val multipleLetStatements = "let count = 5 ; let sum = 10 ;"
    val multipleLetTokens: List[LexicalElem] =
      List(k("let"), id("count"), sym('='), int(5), sym(';'), k("let"), id("sum"), sym('='), int(10), sym(';'))

    val doStatement = "do Output . print ( myChar ) ;"
    val doTokens: List[LexicalElem] =
      List(k("do"), id("Output"), sym('.'), id("print"), sym('('), id("myChar"), sym(')'), sym(';'))

    val returnStatement = "return ;"
    val returnTokens: List[LexicalElem] = List(k("return"), sym(';'))
  }

  "compileSubroutineBody" should "compile a valid subroutine body" in {
    import CompileSubroutineBodyHelper.*

    val input = wrapCurly(List(multipleVarDecs, multipleLetStatements, doStatement, returnStatement).mkString(" "))
    val tokeniser = testTokeniser(input)

    val expectedTokens = wrapCurly(multipleVarDecsTokens ++ multipleLetTokens ++ doTokens ++ returnTokens)
    CompilationEngine.compileSubroutineBody(tokeniser) shouldBe Right(expectedTokens)
  }

  it should "compile a subroutine body with an arbitrary number of var decs" in {
    import CompileSubroutineBodyHelper.*

    val table = Table[Option[List[String]], List[LexicalElem]](
      ("varDecs", "expectedStatementSymbols"),
      (Some(List(multipleVarDecs)), multipleVarDecsTokens),
      (Some(List(singleVarDec)), singleVarDecTokens),
      (Some(List(singleVarDec, multipleVarDecs)), singleVarDecTokens ++ multipleVarDecsTokens),
      (None, List())
    )

    forAll(table) { case (maybeVarDecsList, expectedSymbols) =>
      val statementsString = maybeVarDecsList match {
        case Some(list) => list.mkString(" ")
        case None => ""
      }
      val input = wrapCurly(statementsString)
      val expectedOutput = wrapCurly(expectedSymbols)
      CompilationEngine.compileSubroutineBody(testTokeniser(input)) shouldBe Right(expectedOutput)
    }
  }

  it should "compile a subroutine body with an arbitrary number of statements" in {
    import CompileSubroutineBodyHelper.*

    val table = Table[Option[List[String]], List[LexicalElem]](
      ("statements", "expectedStatementSymbols"),
      (Some(List(returnStatement)), returnTokens),
      (Some(List(multipleLetStatements, doStatement, returnStatement)), multipleLetTokens ++ doTokens ++ returnTokens),
      (Some(List(multipleLetStatements, doStatement)), multipleLetTokens ++ doTokens),
      (None, List())
    )

    forAll(table) { case (maybeStatementsList, expectedStatementSymbols) =>
      val statementsString = maybeStatementsList match {
        case Some(list) => list.mkString(" ")
        case None => ""
      }
      val input = wrapCurly(statementsString)
      val expectedOutput = wrapCurly(expectedStatementSymbols)
      CompilationEngine.compileSubroutineBody(testTokeniser(input)) shouldBe Right(expectedOutput)
    }
  }

  object CompileSubroutineHelper {
    import CompileSubroutineBodyHelper.*

    val methodDeclaration = "method int myMethod ( )"
    val methodDeclarationTokens: List[LexicalElem] = List(k("method"), k("int"), id("myMethod"), sym('('), sym(')'))

    val functionDeclaration = "function void myFunction ( boolean a )"
    val functionDeclarationTokens: List[LexicalElem] = List(k("function"), k("void"), id("myFunction")) ++ wrapBracket(List(k("boolean"), id("a")))

    val constructorDeclaration = "constructor MyClass constructMyClass ( char a )"
    val constructorDeclarationTokens: List[LexicalElem] =
      List(k("constructor"), id("MyClass"), id("constructMyClass")) ++
        wrapBracket(List(k("char"), id("a")))

    val methodDeclarationWithInvalidReturnType = "method class myMethod ( )"

    val returnBody = wrapCurly(returnStatement)
    val returnBodyTokens: List[LexicalElem] = wrapCurly(returnTokens)
  }

  "compileSubroutine" should "compile a valid subroutine" in {
    import CompileSubroutineHelper.*

    val table = Table(
      ("subroutineDeclaration", "expectedSubroutineDeclarationTokens"),
      (methodDeclaration, methodDeclarationTokens),
      (functionDeclaration, functionDeclarationTokens),
      (constructorDeclaration, constructorDeclarationTokens)
    )

    forAll(table){ case (subroutineDeclaration, expectedSubroutineDeclarationTokens) =>
      val input = subroutineDeclaration + " " + returnBody
      val expectedSymbols = expectedSubroutineDeclarationTokens ++ returnBodyTokens

      CompilationEngine.compileSubroutine(testTokeniser(input)) shouldBe Right(expectedSymbols)
    }
  }

  it should "not compile a subroutine with an invalid return type" in {
    import CompileSubroutineHelper.*

    val input = methodDeclarationWithInvalidReturnType + " " + returnBody

    CompilationEngine.compileSubroutine(testTokeniser(input)) shouldBe Left("keyword class cannot be used as a return type")
  }

  object CompileClassHelper {
    import CompileSubroutineHelper.*

    val classDec = "class myClass"
    val classDecTokens = List(k("class"), id("myClass"))

    val fieldDecs = "static int myInt , myCount ; field char myChar ;"
    val fieldDecTokens = List(k("static"), k("int"), id("myInt"), sym(','), id("myCount"), sym(';'), k("field"), k("char"), id("myChar"), sym(';'))

    val methodDec = methodDeclaration + " " + returnBody
    val methodDecTokens = methodDeclarationTokens ++ returnBodyTokens

    val functionDec = functionDeclaration + " " + returnBody
    val functionDecTokens = functionDeclarationTokens ++ returnBodyTokens
  }

  "compileClass" should "compile a class with an arbitrary number of subroutines" in {
    import CompileClassHelper.*

    val data = Table(
      ("maybeSubroutineDec", "subroutineDecTokens"),
      (Some(methodDec), methodDecTokens),
      (Some(methodDec + " " + functionDec), methodDecTokens ++ functionDecTokens),
      (None, List())
    )

    forAll(data){ case (maybeSubroutineDec, subroutineDecTokens) =>
      val subroutineDecString = maybeSubroutineDec.map(" " + _).getOrElse("")
      val input = classDec + " " + wrapCurly(fieldDecs + subroutineDecString)
      val expectedTokens = classDecTokens ++ wrapCurly(fieldDecTokens ++ subroutineDecTokens)

      CompilationEngine.compileClass(testTokeniser(input)) shouldBe Right(expectedTokens)
    }
  }

  "compileTerm" should "compile a simple term" in {
    val data = Table(
      ("term", "expectedLexElems"),
      ("100", List(int(100))),
      ("\"foo\"", List(str("foo"))),
      ("true", List(Keyword("true"))),
      ("false", List(Keyword("false"))),
      ("null", List(Keyword("null"))),
      ("this", List(Keyword("this"))),
      ("myVar", List(id("myVar"))),
//      ("myVar [ 100 ]", List(id("myVar"), sym('['), int(100), sym(']'))) //TODO
//      ("- 100", List(UnaryOperator('-'), int(100))),
//      ("myVar . myMethod ( )", List(id("myVar"), sym('.'), id("myMethod"), sym('('), sym(')'))),
//      ("myMethod ( )", List(id("myMethod"), sym('('), sym(')'))),
//      ("( 100 )", List(sym('('), int(100), sym(')')))
    )

    forAll(data){ case (term, expectedLexElems) =>
      CompilationEngine.compileTerm(testTokeniser(term)) shouldBe Right(expectedLexElems)
    }
  }

  it should "return an error for an invalid term" in {
    val data = Table(
      ("invalidTerm", "expectedError"),
      ("class", "Cannot create keyword const from keyword class")
    )

    forAll(data) { case (invalidTerm, expectedError) =>
      CompilationEngine.compileTerm(testTokeniser(invalidTerm)) shouldBe Left(expectedError)
    }
  }

  private def wrapCurly(s: String) = if (s.nonEmpty) "{ " + s + " }" else "{ }"
  private def wrapCurly(l: List[LexicalElem]) = (sym('{') +: l) :+ sym('}')
  private def wrapCurly(elems: LexicalElem*): List[LexicalElem] = (sym('{') +: elems.toList) :+ sym('}')
  private def wrapBracket(s: String) =  if (s.nonEmpty) "( " + s + " )" else "( )"
  private def wrapBracket(l: List[LexicalElem]) = (sym('(') +: l) :+ sym(')')
  private def wrapBracket(elems: LexicalElem*): List[LexicalElem] = (sym('(') +: elems.toList) :+ sym(')')

  "compile expression" should "compile a valid expression" in {
    val data = Table(
      ("expression", "expected"),
      ("a", List(id("a"))),
      ("\"b\"", List(str("b"))),
      ("100", List(int(100))),
      ("a * 100", List(id("a"), sym('*'), int(100))),
      ("( a + 100 ) * 10", wrapBracket(List(id("a"), sym('+'), int(100))) ++ List(sym('*'), int(10))),
      ("( ~ 10 * 100 ) / 1000", wrapBracket(List(sym('~'), int(10), sym('*'), int(100))) ++ List(sym('/'), int(1000)))
    )

    forAll(data) { case (expression, expected) =>
      CompilationEngine.compileExpression(testTokeniser(expression)) shouldBe Right(expected)
    }
  }

  class FakeTokeniser(var tokens: List[String]) extends Tokeniser {
    override def advance(): Unit = tokens = tokens.tail

    def allTokens: Seq[String] = tokens.tail

    override def currentToken: String = tokens.head

    override def hasMoreTokens: Boolean = tokens.nonEmpty
  }

  def testTokeniser(l: List[String]) = new FakeTokeniser(l)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
