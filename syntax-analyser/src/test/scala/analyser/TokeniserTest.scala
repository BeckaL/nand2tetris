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


    
    //TODO: ensure comments are ignored
}