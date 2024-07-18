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
  }

  "main" should "produce the expected output file" in {
    Main.main(Array("./src/test/resources/1/DoAndLetProg.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/1/ExpectedDoAndLetProg.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/1/DoAndLetProg.xml")

    actualOutput shouldBe expectedOutput
  }

  private def readLinesWithoutWhitespace(input: String): List[String] =
    readFile(input).map(_.replaceAll(" ", ""))

}
