package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ParserTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "parse" should "parse an instruction correctly" in {
    val data = Table(
      ("instruction", "expectedCommand"),
      ("add", Add),
      ("sub", Sub),
      ("neg", Neg),
      ("eq", Eq),
      ("gt", Gt),
      ("lt", Lt),
      ("and", And),
      ("or", Or),
      ("not", Not),
      ("pop local 5", Pop(LCL, 5)),
      ("push temp 10", Push(TEMP, 10)),
      ("function SimpleFunction.test 2", FunctionDeclaration("SimpleFunction.test", 2)),
      ("return", FunctionReturn),
      ("call SimpleFunction.test 2", FunctionCall("SimpleFunction.test", 2)),
      ("label LABEL_NAME.FOO", Label("LABEL_NAME.FOO")),
      ("goto LABEL_NAME.FOO", GoTo("LABEL_NAME.FOO")),
      ("if-goto LABEL_NAME.FOO", IfGoTo("LABEL_NAME.FOO")),
    )

    forAll(data) { case (instruction, expectedCommand) =>
      Parser.parse(List(instruction)) shouldBe List(expectedCommand)
    }
  }

  it should "parse a list of instructions, ignoring comments and whitespace" in {
    val instructions = List(
      "// some comment",
      "// some more comment",
      "",
      "  ",
      "   push constant 10",
      " push constant 11",
      "add  // adds 10 and 11 ",
      "  pop temp 2   "
    )

    val expected = List(Push(CONSTANT, 10), Push(CONSTANT, 11), Add, Pop(TEMP, 2))

    Parser.parse(instructions) shouldBe expected
  }

  //since we can assume correct inputs, I'm not worrying too much about error handling for now
   it should "throw runtime exceptions for invalid commands" in {
    val data = Table(
      ("instruction", "expectedErrorMessage"),
      ("notACommand", "couldn't parse string notACommand into command"),
      ("add addDoesntTakeArguments", "couldn't parse string add addDoesntTakeArguments into command"),
      ("push notAMemorySegment 5", "couldn't parse string notAMemorySegment into memory segment"),
      ("pop local i", "couldn't parse string pop local i into command"),
    )

    forAll(data) { case (instruction, expectedErrorMessage) =>
      the[RuntimeException] thrownBy (Parser.parse(List(instruction))) should have message expectedErrorMessage
    }

  }
}
