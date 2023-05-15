package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompilationEngineTest extends AnyFlatSpec with Matchers {

  "compileLet" should "compile a valid let statement" in {
    val tokeniser = testTokeniser(List("let", "count", "=", "count"))
    CompilationEngine.compileLet(tokeniser) shouldBe Right(LetStatement(VarName("count"), VarName("count")))
  }

  it should "throw an error on an invalid let statement" in {
    val tokeniser = testTokeniser(List("let", "count", "*", "count"))
    CompilationEngine.compileLet(tokeniser) shouldBe Left("uh-oh")
  }

  class FakeTokeniser(var tokens: List[String]) extends Tokeniser {
    override def advance() = { tokens = tokens.tail }

    override def currentToken = tokens.head
  }

  def testTokeniser(t: List[String]) = new FakeTokeniser(t)
}
