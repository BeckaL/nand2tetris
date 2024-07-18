package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class DefaultTokeniserTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

    "hasMoreTokens" should "return true or false depending on whether there are tokens remaining" in {
        val input = "while (count < 100) { let count = count; } }  "

        val positionsAndExpectedResults = Table(
            ("position", "expectedResult"),
            (0, true),
            (5, true),
            (19, true),
            (44, false),
            (46, false)
        )

        forAll(positionsAndExpectedResults) { case (position, expectedResult) =>
            val tokeniser = new DefaultTokeniser(input, position)
            tokeniser.hasMoreTokens shouldBe expectedResult
        }
    }

    "advance" should "correctly set the next position and token" in {
        val input = " while( co_nt < 100) { let count = count; let a = \"b\"; } }  "

        val expectedTokensAndPositions = List(
            ("while", 6),
            ("(", 7),
            ("co_nt", 13),
            ("<", 15),
            ("100", 19),
            (")", 20),
            ("{", 22),
            ("let", 26),
            ("count", 32),
            ("=", 34),
            ("count", 40),
            (";", 41),
            ("let", 45),
            ("a", 47),
            ("=", 49),
            ("\"b\"", 53),
            (";", 54),
            ("}", 56),
            ("}", 58)
        )

        val tokeniser = new DefaultTokeniser(input, 0)

        expectedTokensAndPositions.indices.foreach { i =>
          tokeniser.advance()
          val (expectedToken, expectedPosition) = expectedTokensAndPositions(i)
          tokeniser.position shouldBe expectedPosition
          tokeniser.currentToken shouldBe expectedToken
        }
    }

    "advance" should "correctly set the next position and token 2" in {
        val input = "let average = 4;"

        val expectedTokensAndPositions = List(
            ("let", 3, true),
            ("average", 11, true),
            ("=", 13, true),
            ("4", 15, true),
            (";", 16, false),
        )

        val tokeniser = new DefaultTokeniser(input, 0)

        expectedTokensAndPositions.indices.foreach { i =>
            tokeniser.advance()
            val (expectedToken, expectedPosition, expectedHasMoreTokens) = expectedTokensAndPositions(i)
            tokeniser.position shouldBe expectedPosition
            tokeniser.currentToken shouldBe expectedToken
            tokeniser.hasMoreTokens shouldBe expectedHasMoreTokens
        }

    }


    "value return methods" should "work appropriately" in {
        val tokeniser = new DefaultTokeniser("", 0)

        tokeniser.currentToken = "class"
        tokeniser.keyword shouldBe "class"

        tokeniser.currentToken = "100"
        tokeniser.intVal shouldBe 100

        tokeniser.currentToken = "a_var"
        tokeniser.identifier shouldBe "a_var"

        tokeniser.currentToken = "{"
        tokeniser.symbol shouldBe '{'

        tokeniser.currentToken = "\"a_string\""
        tokeniser.stringVal shouldBe "a_string"
    }
}