import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class LexerTest extends AnyFlatSpec with Matchers {
  "lex" should "return a symbols map and a list of lines stripped of any labels" in {
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

    val expectedMap = Lexer.PREDEFINED_SYMBOLS ++ Map("LOOP_BLAH" -> 4, "STOP" -> 18, "i.init" -> 16, "sum" -> 17)

    Lexer.getMap(instructions) shouldBe expectedMap
  }

}
