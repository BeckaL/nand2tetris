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

  //TODO
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
      val tokeniser = testTokeniser(statementString)
      CompilationEngine.compileLet(tokeniser) shouldBe Right(expectedTokens)
    }
  }




  //invalid compile lets
  //"let count ;"
  //"let 5 = 10 ;"
  //"let foo = bar"
  //"let foo * bar ; "

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

    override def hasMoreTokens: Boolean = tokens.nonEmpty
  }

  def testTokeniser(l: List[String]) = new FakeTokeniser(l)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
