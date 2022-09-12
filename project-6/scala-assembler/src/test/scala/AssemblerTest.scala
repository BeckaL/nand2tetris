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
     Assembler.parseAsBinaryString(List(instruction), Lexer.PREDEFINED_SYMBOLS) shouldBe List(expected)
   }
 }
  it should "ignore commented lines and whitespace lines" in {
    val instructions = List(" ", "@51", "// some comments", "   // some other comments", "(ASYMBOL)", "@1000", "     ")
    val expected = List(fiftyOneInBinary, aThousandInBinary)
    Assembler.parseAsBinaryString(instructions, Lexer.PREDEFINED_SYMBOLS) shouldBe expected
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
      Assembler.parseAsBinaryString(List(instruction), Lexer.PREDEFINED_SYMBOLS) shouldBe List(expected)
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
      Assembler.parseAsBinaryString(List(instruction), Lexer.PREDEFINED_SYMBOLS) shouldBe List(expected)
    }
  }

  val dEqualsAInBinary = "1110110000010000"

  val fiftyOneInBinary = "0000000000110011"
  val aThousandInBinary = "0000001111101000"
}
