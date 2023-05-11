package analyser

import InputUtils.stripInput
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class InputUtilsTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "stripInput" should "remove any type of comment" in {
    val inputWithCommentsToEndOfLine = List(
      "while (count < 100) // define a basic while loop",
      "{ let count = count } // do something",
      "} //end the loop"
    )

    val inputWithMultiLineComment = List(
      "while (count < 100) ",
      " /* define a loop",
      "  then do some stuff",
      "then end it */",
      "{ let count = count } ",
      "} //end the loop"
    )

    val inputWithMultilineApiComment = List(
      "while (count < 100) ",
      " /** define a loop",
      "  then do some stuff",
      "then end it */",
      "{ let count = count } ",
      "} //end the loop"
    )

    val data = Table("input",
      inputWithCommentsToEndOfLine,
      inputWithMultiLineComment,
      inputWithMultilineApiComment
    )

    val expectedStrippedInput = List(
      "while (count < 100) ",
      "{ let count = count } ",
      "} "
    )

    forAll(data) { input =>
      stripInput(input) shouldBe expectedStrippedInput
    }
  }

  it should "handle multiline comments that end in the same line" in {
    val input = List(
      "while /* a single line multiline comment */(count < 100) ",
      "{ let count = count /** we're assigning count to count */} ",
      "} "
    )

    val expectedStrippedInput = List(
      "while (count < 100) ",
      "{ let count = count } ",
      "} "
    )

    stripInput(input) shouldBe expectedStrippedInput
  }

  it should "strip comments according to the first comment token it encounters, ignoring any other comment tokens contained within that comment" in {
    val inputWithMultilineCommentsToEndOfLine = List(
      "while (count < 100) // define a basic while loop /* this is not a multiline comment",
      "{ let count = count } */ this should not be considered the end of a multiline comment",
      "} //end the loop"
    )

    val expectedStrippedInputWithMultilineCommentsToEndOfLine = List(
      "while (count < 100) ",
      "{ let count = count } */ this should not be considered the end of a multiline comment",
      "} "
    )

    val inputWithApiCommentsToEndOfLine = List(
      "while (count < 100) // define a basic while loop /** this is not a multiline api comment",
      "{ let count = count } */ this should not be considered the end of a multiline comment",
      "} //end the loop"
    )

    val expectedStrippedInputWithApiCommentsToEndOfLine = List(
      "while (count < 100) ",
      "{ let count = count } */ this should not be considered the end of a multiline comment",
      "} "
    )

    val inputWithSingleLineCommentTokenInMultiLineComment = List(
      "while (count < 100) ",
      " /* define a loop //this comment string should not override the multiline comment",
      "  then do some stuff",
      "then end it */",
      "{ let count = count } ",
      "} //end the loop"
    )

    val expectedStrippedInputWithSingleLineCommentTokenInMultiLineComment = List(
      "while (count < 100) ",
      "{ let count = count } ",
      "} "
    )


    val data = Table(("input", "expectedOutput"),
      (inputWithMultilineCommentsToEndOfLine, expectedStrippedInputWithMultilineCommentsToEndOfLine),
      (inputWithApiCommentsToEndOfLine, expectedStrippedInputWithApiCommentsToEndOfLine),
      (inputWithSingleLineCommentTokenInMultiLineComment, expectedStrippedInputWithSingleLineCommentTokenInMultiLineComment)
    )

    forAll(data) { case (input, expectedOutput) =>
      stripInput(input) shouldBe expectedOutput
    }
  }
}
