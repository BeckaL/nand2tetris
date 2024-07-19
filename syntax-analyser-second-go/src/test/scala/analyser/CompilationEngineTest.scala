package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CompilationEngineTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "compileLet" should "compile a valid let statement" in {
    val tokeniser = testTokeniser("let count = count ;")
    CompilationEngine.compileLet(tokeniser) shouldBe Right(List(Keyword("let"), LexicalIdentifier("count"), LexicalSymbol('='), LexicalIdentifier("count"), LexicalSymbol(';')))
  }

  it should "compile a valid let statement with an integer constant" in {
    val tokeniser = testTokeniser("let count = 100 ;")
    CompilationEngine.compileLet(tokeniser) shouldBe Right(List(Keyword("let"), LexicalIdentifier("count"), LexicalSymbol('='), LexicalIntegerConstant(100), LexicalSymbol(';')))
  }

  it should "throw an error on an invalid let statement" in {
    val tokeniser = testTokeniser("let count * count")
    CompilationEngine.compileLet(tokeniser) shouldBe Left("uh-oh, expected * to equal =")
  }

  it should "compile a do statement with a single expression and no params" in {
    val tokeniser = testTokeniser("do square . dispose ( ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(List(Keyword("do"), LexicalIdentifier("square"), LexicalSymbol('.'), LexicalIdentifier("dispose"), LexicalSymbol('('), LexicalSymbol(')'), LexicalSymbol(';')))
  }

  it should "compile a do statement with a single param" in {
    val tokeniser = testTokeniser("do Memory . deAlloc ( square ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(List(Keyword("do"), LexicalIdentifier("Memory"), LexicalSymbol('.'), LexicalIdentifier("deAlloc"), LexicalSymbol('('), LexicalIdentifier("square"), LexicalSymbol(')'), LexicalSymbol(';')))
  }

  it should "compile a do statement with multiple params" in {
    val tokeniser = testTokeniser("do Foo . bar ( 100 , \"b\" , c ) ;")
    CompilationEngine.compileDo(tokeniser) shouldBe Right(
      List(Keyword("do"), LexicalIdentifier("Foo"), LexicalSymbol('.'), LexicalIdentifier("bar"), LexicalSymbol('('), LexicalIntegerConstant(100), LexicalSymbol(','), LexicalStringConstant("b"), LexicalSymbol(','), LexicalIdentifier("c"), LexicalSymbol(')'), LexicalSymbol(';')))
  }

  "compileVarDec" should "compile a varDeclaration" in {
    val tokeniser = testTokeniser("var int average ;")
    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(List(Keyword("var"), Keyword("int"), LexicalIdentifier("average"), LexicalSymbol(';')))
  }

  it should "compile a varDeclarationWithMultipleVars" in {
    val tokeniser = testTokeniser("var int average , sum , count ;")
    CompilationEngine.compileVarDec(tokeniser) shouldBe Right(
      List(Keyword("var"), Keyword("int"), LexicalIdentifier("average"), LexicalSymbol(','), LexicalIdentifier("sum"), LexicalSymbol(','), LexicalIdentifier("count"), LexicalSymbol(';'))
    )
  }

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
    override def advance() = { tokens = tokens.tail }

    override def currentToken = tokens.head

    override def hasMoreTokens: Boolean = tokens.size > 0
  }

  def testTokeniser(t: List[String]) = new FakeTokeniser(t)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
