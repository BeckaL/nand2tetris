package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CompilationEngineTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private def k(s: String) = Keyword(s)

  private def sym(c: Char) = LexicalSymbol(c)

  private def id(s: String) = LexicalIdentifier(s)

  private def int(n: Int) = LexicalIntegerConstant(n)

  private def str(s: String) = LexicalStringConstant(s)

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
    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(
      List(k("var"), k("int"), id("average"), sym(';')))
  }

  it should "compile a varDeclarationWithMultipleVars" in {
    val tokeniser = testTokeniser("var int average , sum , count ;")
    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(
      List(k("var"), k("int"), id("average"), sym(','), id("sum"), sym(','), id("count"), sym(';'))
    )
  }

  "compile return" should "compile an empty return statement" in {
    val tokeniser = testTokeniser("return ;")
    CompilationEngine.compileReturn(tokeniser) shouldBe Right(
      List(k("return"), sym(';'))
    )
  }

  it should "return an error for an invalid return" in {
    val tokeniser = testTokeniser("return + ;")
    CompilationEngine.compileReturn(tokeniser) shouldBe Left(
      "uh-oh, expected + to equal ;"
    )
  }

  "compileParameterList" should "compile an empty parameter list" in {
    val tokeniser = testTokeniser("( )")
    CompilationEngine.compileParameterList(tokeniser) shouldBe Right(
      wrapBracket(List())
    )
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
    val singleVarDecTokens: List[LexicalElement] =
      List(k("var"), id("MyClass"), id("blah"), sym(';'));

    val multipleVarDecs = "var int count , sum ; var char myChar ;"
    val multipleVarDecsTokens: List[LexicalElement] =
      List(k("var"), k("int"), id("count"), sym(','), id("sum"), sym(';'), k("var"), k("char"), id("myChar"), sym(';'))

    val multipleLetStatements = "let count = 5 ; let sum = 10 ;"
    val multipleLetTokens: List[LexicalElement] =
      List(k("let"), id("count"), sym('='), int(5), sym(';'), k("let"), id("sum"), sym('='), int(10), sym(';'))

    val doStatement = "do Output . print ( myChar ) ;"
    val doTokens: List[LexicalElement] =
      List(k("do"), id("Output"), sym('.'), id("print"), sym('('), id("myChar"), sym(')'), sym(';'))

    val returnStatement = "return ;"
    val returnTokens: List[LexicalElement] = List(k("return"), sym(';'))
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

    val table = Table[Option[List[String]], List[LexicalElement]](
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

    val table = Table[Option[List[String]], List[LexicalElement]](
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
    val methodDeclarationTokens: List[LexicalElement] = List(k("method"), k("int"), id("myMethod"), sym('('), sym(')'))

    val functionDeclaration = "function void myFunction ( boolean a )"
    val functionDeclarationTokens: List[LexicalElement] = List(k("function"), k("void"), id("myFunction")) ++ wrapBracket(List(k("boolean"), id("a")))

    val constructorDeclaration = "constructor MyClass constructMyClass ( char a )"
    val constructorDeclarationTokens: List[LexicalElement] =
      List(k("constructor"), id("MyClass"), id("constructMyClass")) ++
        wrapBracket(List(k("char"), id("a")))

    val methodDeclarationWithInvalidReturnType = "method class myMethod ( )"

    val returnBody = wrapCurly(returnStatement)
    val returnBodyTokens: List[LexicalElement] = wrapCurly(returnTokens)
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

  private def wrapCurly(s: String) = if (s.nonEmpty) "{ " + s + " }" else "{ }"

  private def wrapCurly(l: List[LexicalElement]) = (sym('{') +: l) :+ sym('}')
  private def wrapBracket(s: String) =  if (s.nonEmpty) "( " + s + " )" else "( )"
  private def wrapBracket(l: List[LexicalElement]) = (sym('(') +: l) :+ sym(')')

  "compile expression" should "compile a valid expression" in {
    def variable(s: String) = VarName(s)

    def exp(t: Term, o: Option[(Operator, Term)]) = Expression(t, o)

    def int(i: Integer) = IntegerConstant(i)

    def string(s: String) = StringConstant(s)

    def op(s: String) = Operator(s)

    val data = Table(
      ("expression", "expected"),
      (List("a"), exp(variable("a"), None)),
      (List("\"b\""), exp(string("b"), None)),
      (List("100"), exp(int(100), None)),
      (List("a", "*", "100"), exp(variable("a"), Some((op("*"), int(100))))),
      //      (List("(", "a", "+", "100", ")", "*", "10"), exp())
    )

    forAll(data) { case (expression, expected) =>
      CompilationEngine.compileExpression(testTokeniser(expression)) shouldBe Right(expected)
    }
  }

  //  class SquareGame {
  //    field Square square;
  //    field int direction;
  //
  //    constructor SquareGame new() {
  //      let square = square;
  //      let direction = direction;
  //      return square;
  //    }
  //
  //    method void dispose() {
  //      do square.dispose();
  //      do Memory.deAlloc(square);
  //      return;
  //    }
  //
  //    method void moveSquare() {
  //      if (direction) { do square.moveUp(); }
  //      if (direction) { do square.moveDown(); }
  //      if (direction) { do square.moveLeft(); }
  //      if (direction) { do square.moveRight(); }
  //      do Sys.wait(direction);
  //      return;
  //    }class SquareGame {
  //      field Square square;
  //      field int direction;
  //
  //      constructor SquareGame new() {
  //        let square = square;
  //        let direction = direction;
  //        return square;
  //      }
  //
  //      method void dispose() {
  //        do square.dispose();
  //        do Memory.deAlloc(square);
  //        return;
  //      }
  //
  //      method void moveSquare() {
  //        if (direction) { do square.moveUp(); }
  //        if (direction) { do square.moveDown(); }
  //        if (direction) { do square.moveLeft(); }
  //        if (direction) { do square.moveRight(); }
  //        do Sys.wait(direction);
  //        return;
  //      }

  class FakeTokeniser(var tokens: List[String]) extends Tokeniser {
    override def advance() = {
      tokens = tokens.tail
    }

    def allTokens: Seq[String] = tokens.tail

    override def currentToken = tokens.head

    override def hasMoreTokens: Boolean = tokens.size > 0
  }

  def testTokeniser(t: List[String]) = new FakeTokeniser(t)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
