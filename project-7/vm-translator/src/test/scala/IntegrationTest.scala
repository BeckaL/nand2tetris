import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.{Files, Paths}
import org.scalatest.BeforeAndAfterEach


class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach{
  val fileNames = List("SimpleAdd", "StackTest", "StaticTest", "BasicTest", "PointerTest")

  "main" should "produce the expected asm file" in {
    val data = Table(
      ("file", "expectedFile"),
      ("SimpleAdd", "ExpectedSimpleAdd"),
      ("StackTest", "ExpectedStackTest"),
      ("StaticTest", "ExpectedStaticTest"),
      ("BasicTest", "ExpectedBasicTest"),
      ("PointerTest", "ExpectedPointerTest")
    )

    forAll(data) { case (fileName, expectedFilename) =>
      val runCommands = Array("./src/test/resources", fileName)

      Main.main(runCommands)

      val expected = FileOps.readFile(s"./src/test/resources/$fileName.asm")
      val actual = FileOps.readFile(s"./src/test/resources/$expectedFilename.asm")
      actual shouldBe expected
    }
  }

  override def afterEach(): Unit = {
    fileNames.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources/$fileName.asm")))
  }
}

