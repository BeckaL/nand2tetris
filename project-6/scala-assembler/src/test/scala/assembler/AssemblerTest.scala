package assembler

import assembler.{Assembler, Constants}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AssemblerTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
 "parseAsBinaryString" should "parse an a instruction" in {
   val data = Table(
     ("instruction", "expected"),
     ("@51", fiftyOneInBinary),
     ("@51 // some comment", fiftyOneInBinary),
     ("@51  ", fiftyOneInBinary),
     ("@1000", aThousandInBinary),
     (" @R1   ", "0000000000000001"),
     ("@R14  ", "0000000000001110"),
     (" @THIS", "0000000000000011"),
     (" @KBD // selects the keyboard", "0110000000000000")
   )

   forAll(data) { case (instruction, expected) =>
     Assembler.parseAsBinaryString(List(instruction), Constants.PREDEFINED_SYMBOLS) shouldBe List(expected)
   }
 }

  it should "ignore commented lines and whitespace lines" in {
    val instructions = List(" ", "@51", "// some comments", "   // some other comments", "(ASYMBOL)", "@1000", "     ")
    val expected = List(fiftyOneInBinary, aThousandInBinary)
    Assembler.parseAsBinaryString(instructions, Constants.PREDEFINED_SYMBOLS) shouldBe expected
  }

  it should "translate a c instruction without a jump" in {
    val data = Table(
      ("instruction", "expected"),
      ("D=A", "1110110000010000"),
      ("D=M", "1111110000010000"),
      ("M=D   // M[2] = D", "1110001100001000"),
      ("D=D-M", "1111010011010000"),
      ("-D //some comment", "1110001111000000"),
      ("   A=!M // some comment", "1111110001100000")
    )

    forAll(data) { case (instruction, expected) =>
      Assembler.parseAsBinaryString(List(instruction), Constants.PREDEFINED_SYMBOLS) shouldBe List(expected)
    }
  }

  it should "translate a c instruction with a jump" in {
    val data = Table(
      ("instruction", "expected"),
      ("D=A;JGT", "1110110000010001"),
      ("D=M ;JEQ  ", "1111110000010010"),
      ("D=D-M;   JGE", "1111010011010011"),
      ("-D ; JLT //some comment", "1110001111000100"),
      ("   A=!M;JNE // some comment", "1111110001100101"),
      ("  0   ; JLE", "1110101010000110"),
      ("1;JMP", "1110111111000111")
    )

    forAll(data) { case (instruction, expected) =>
      Assembler.parseAsBinaryString(List(instruction), Constants.PREDEFINED_SYMBOLS) shouldBe List(expected)
    }
  }

  "getMap" should "return a symbols map and a list of lines stripped of any labels" in {
    val instructions = List(
      "//a comment",
      "  @i.init", // line 0, will be assigned to R16
      "  M=1", //line 1
      " //sum = 0",
      "  @sum", // line 2
      "  M=0", // line 3
      "(LOOP_BLAH)", // will refer to 4 in map
      " // a comment",
      "  @i.init", // line 4
      "  D=M", // line 5
      "   @R0", // line 6
      "  D=D-M", // line 7
      "  @STOP", // line 8
      "  D; JGT", // line 9
      " // sum += i",
      "  @i.init", // line 10
      " D=M", // line 11
      "  @sum", // line 12
      " M=D+M", // line 13
      "  //i ++",
      "  @i.init", // line 14
      "M=M+1", // line 15
      "  @LOOP_BLAH //a comment", // line 16
      "0;JMP",  // line 17
      "  (STOP)", //wil refer to 18
      "   @sum", // line 18
      " D=M" // line 19
    )

    val expectedMap = Constants.PREDEFINED_SYMBOLS ++ Map("LOOP_BLAH" -> 4, "STOP" -> 18, "i.init" -> 16, "sum" -> 17)

    Assembler.getMap(instructions) shouldBe expectedMap
  }

  val dEqualsAInBinary = "1110110000010000"

  val fiftyOneInBinary = "0000000000110011"
  val aThousandInBinary = "0000001111101000"
}
