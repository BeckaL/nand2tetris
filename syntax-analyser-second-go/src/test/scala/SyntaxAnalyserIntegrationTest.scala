import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import inputoutput.FileOps.*
import org.scalatest.BeforeAndAfterEach

class SyntaxAnalyserIntegrationTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  override
  def beforeEach(): Unit = {
    Files.deleteIfExists(Paths.get("./src/test/resources/1/DoAndLetProg.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/2/SimpleProg.xml"))
    Files.deleteIfExists(Paths.get("./src/test/resources/3/ExpectedReducedMain.xml"))
  }

  "main" should "produce the expected output file for a simple set of statements" in {
    Main.main(Array("./src/test/resources/1/DoAndLetProg.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedDoAndLetProg.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/DoAndLetProg.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "produce the expected output file for a simple programme" in {
    Main.main(Array("./src/test/resources/2/SimpleProg.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/2/ExpectedSimpleProg.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/2/SimpleProg.xml")

    actualOutput shouldBe expectedOutput
  }

  it should "produce the expected output file for a class" in {
    pending //TODO
    Main.main(Array("./src/test/resources/3/ReducedMain.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/3/ExpectedReducedMain.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/3/ReducedMain.xml")

    actualOutput shouldBe expectedOutput
  }

  private def readLinesWithoutWhitespace(input: String): List[String] =
    readFile(input).map(_.replaceAll(" ", ""))

}
