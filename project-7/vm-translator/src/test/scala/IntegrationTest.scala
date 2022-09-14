import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.{Files, Paths}
import org.scalatest.BeforeAndAfterEach


class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach{
  val fileNames = List("SimpleAdd", "StackTest")

  "main" should "produce the expected asm file" in {
    val data = Table(
      ("file", "expectedFile"),
      ("SimpleAdd", "ExpectedSimpleAdd"),
      ("StackTest", "ExpectedStackTest")
    )

    forAll(data) { case (fileName, expectedFilename) =>
      val runCommands = List("./src/test/resources", fileName)

      Main.run(runCommands)

      val expected = FileOps.readFile(s"./src/test/resources/$fileName.asm")
      val actual = FileOps.readFile(s"./src/test/resources/$expectedFilename.asm")
      actual shouldBe expected
    }
  }

  override def afterEach(): Unit = {
    fileNames.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources/$fileName.asm")))
  }
}

