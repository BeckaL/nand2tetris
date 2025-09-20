import inputoutput.FileOps.readFile
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files,Paths}

class SyntaxAnalyserIntegrationTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  override
  def beforeEach(): Unit = {
    Files.deleteIfExists(Paths.get("./src/test/resources/1/DoAndLetProg.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/2/SimpleProg.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/3/ReducedMain.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/3/Main.xml"))
  }

  "main" should "produce the expected output file for a simple set of statements" in {
    pending
    Main.main(Array("./src/test/resources/1/DoAndLetProg.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedDoAndLetProg.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/DoAndLetProg.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "produce the expected output file for a simple programme" in {
    pending
    Main.main(Array("./src/test/resources/2/SimpleProg.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/2/ExpectedSimpleProg.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/2/SimpleProg.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "produce the expected output file for a class" in {
    pending
    Main.main(Array("./src/test/resources/3/ReducedMain.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/3/ExpectedReducedMain.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/3/ReducedMain.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "produce the expected output file for a full class" in {
    pending
    Main.main(Array("./src/test/resources/4/Main.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/4/ExpectedMain.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/4/Main.xml")

    actualOutput shouldBe expectedOutput
  }

  private def readLinesWithoutWhitespace(input: String): List[String] =
    readFile(input).map(_.replaceAll(" ", "").replaceAll("\t", ""))
}
