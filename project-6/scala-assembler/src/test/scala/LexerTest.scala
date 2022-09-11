import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class LexerTest extends AnyFlatSpec with Matchers {
  "lex" should "return a symbols map and a list of lines stripped of any labels" in {
    val instructions = List(
      "//a comment",
      "  @i", // line 0, will be assigned to R16
      "  M=1", //line 1
      " //sum = 0",
      "  @sum", // line 2
      "  M=0", // line 3
      "(LOOP)", // will refer to 4 in map
      " // a comment",
      "  @i", // line 4
      "  D=M", // line 5
      "   @R0", // line 6
      "  D=D-M", // line 7
      "  @STOP", // line 8
      "  D; JGT", // line 9
      " // sum += i",
      "  @i", // line 10
      " D=M", // line 11
      "  @sum", // line 12
      " M=D+M", // line 13
      "  //i ++",
      "  @i", // line 14
      "M=M+1", // line 15
      "  @LOOP //a comment", // line 16
      "0;JMP",  // line 17
      "  (STOP)", //wil refer to 18
      "   @sum", // line 18
      " D=M" // line 19
    )

    val expectedMap = Lexer.PREDEFINED_SYMBOLS ++ Map("LOOP" -> 4, "STOP" -> 18, "i" -> 16, "sum" -> 17)

    Lexer.getMap(instructions) shouldBe expectedMap
  }

}
