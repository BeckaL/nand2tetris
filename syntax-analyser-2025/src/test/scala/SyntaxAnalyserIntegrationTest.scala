import inputoutput.FileOps.readFile
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files,Paths}

class SyntaxAnalyserIntegrationTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  override
  def beforeEach(): Unit = {
    Files.deleteIfExists(Paths.get("./src/test/resources/1/Square.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/1/Main.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/1/SquareGame.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/2/Square.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/2/Main.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/2/SquareGame.xml"))
  }

  "main" should "produce the expected output" in {
    Main.main(Array("./src/test/resources/1/Square.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedSquare.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/Square.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "work on all files" in {
    Main.main(Array("./src/test/resources/1/Main.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedMain.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/Main.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "work on all files 2" in {
    Main.main(Array("./src/test/resources/1/SquareGame.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedSquareGame.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/SquareGame.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "compile a programme with expressions" in {
    Main.main(Array("./src/test/resources/2/SquareGame.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/2/ExpectedSquareGame.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/2/SquareGame.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "compile a programme with expressions part 2" in {
    Main.main(Array("./src/test/resources/2/Square.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/2/ExpectedSquare.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/2/Square.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "compile a programme with expressions part 3" in {
    Main.main(Array("./src/test/resources/2/Main.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/2/ExpectedMain.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/2/Main.xml")

    actualOutput shouldBe expectedOutput
  }


  private def readLinesWithoutWhitespace(input: String): List[String] =
    readFile(input).map(_.replaceAll(" ", "").replaceAll("\t", ""))
}
