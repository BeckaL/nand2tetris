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

  val validLetStatements = Table(("statement", "expected Tokens"),
    ("let count = count ;", List(k("let"), id("count"), sym('='), id("count"), sym(';'))),
    ("let count = 5 ;", List(k("let"), id("count"), sym('='), int(5), sym(';'))),
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

  //TODO: invalid let cases
  //invalid compile lets
  //"let count ;"
  //"let 5 = 10 ;"
  //"let foo = bar"
  //"let foo * bar ; "
  //"let ",
  //"let ;"
  //"let foo = ";
  //"let foo = ;"

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
