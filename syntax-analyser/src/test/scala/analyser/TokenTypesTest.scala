package analyser

import analyser.TokenTypes
import analyser.TokenTypes.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TokenTypesTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "getTokenType" should "return the token type correctly" in {
    val data = Table(
      ("strings", "expectedTokenType"),
      (List("class", "let", "while"), Keyword),
      (List("(", ")", "{", "}", "<", "=", "-", "+", ";"), Symbol),
      (List("3", "41", "500"), IntConst),
      (List("\"hi\"", "\"str ing\"", "\"\""), StringConst),
      (List("varName", "var_name_2"), Identifier),
    )

    forAll(data) { case (testStrings: List[String], expectedTokenType: TokenType) =>
      testStrings.map(TokenTypes.tokenType).forall(_ == expectedTokenType) shouldBe(true)
    }
  }

  it should "throw for invalid tokens" in {
    val invalidStrings = List(
      "\"\"hi\"", //Strings cannot contain quotation characters inside them
      "\"hi\nmy name is\"", //Strings cannot contain newline characters
      "32768", //Integers cannot be bigger than 32767
      "100000", //Integers cannot be bigger than 32767
      "-1", // Integers cannot be less than zero
      "123.45", //Integers cannot have decimal parts
      "1_a_var", //Identifiers cannot start with a digit
      "not-a-valid-identifier" //Identifiers cannot contain characters other than alphanumeric chars and underscores
    )

    invalidStrings.foreach{ string =>
      val error = intercept[RuntimeException] { TokenTypes.tokenType(string) }
      error.getMessage shouldBe s"Couldn't parse token: ${string} is not a valid token"
    }
  }
}
