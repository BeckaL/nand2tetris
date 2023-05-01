import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import inputoutput.FileOps.*
import org.scalatest.BeforeAndAfterEach

class SyntaxAnalyserIntegrationTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  "main" should "produce the expected output file" in {
    pending
    Main.main(Array("./src/test/resources/ExpressionlessSquare.jack"))

    val expectedOutput = readLinesWithoutWhitespace("./src/test/resources/ExpectedExpressionlessSquare.xml")
    val actualOutput = readLinesWithoutWhitespace("./src/test/resources/ExpressionlessSquare.xml")

    actualOutput shouldBe expectedOutput
  }

  private def readLinesWithoutWhitespace(input: String): List[String] =
    readFile(input).map(_.replaceAll(" ", ""))

  override def afterEach() =
    Files.deleteIfExists(Paths.get("./src/test/resources/ExpressionlessSquare.xml"))
}
