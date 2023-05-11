package analyser

import analyser.Tokeniser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TokeniserTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

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
            val tokeniser = new Tokeniser(input, position)
            tokeniser.hasMoreTokens shouldBe expectedResult
        }
    }

    "advance" should "correctly set the next position and token" in {
        pending
        val input = " while(count < 100) { let count = count; } }  "

        val expectedTokensAndPositions = List(
            ("while", 6),
            ("(", 7),
            ("count", 13),
            ("<", 15),
            ("100", 18),
            (")", 20),
            ("{", 22),
            ("let", 26),
            ("count", 32),
            ("=", 34),
            ("count", 39),
            (";", 41),
            ("}", 43),
            ("}", 44)
        )

        val tokeniser = new Tokeniser(input, 0)

        (0 until expectedTokensAndPositions.size).foreach { i =>
          tokeniser.advance
          val (expectedToken, expectedPosition) = expectedTokensAndPositions(i)
          tokeniser.position shouldBe expectedPosition
          tokeniser.currentToken shouldBe expectedToken
        }
    }
}