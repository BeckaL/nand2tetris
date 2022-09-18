import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.{Files, Paths}
import org.scalatest.BeforeAndAfterEach


class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach{
  val fileNamesForProject7 = List("SimpleAdd", "StackTest", "StaticTest", "BasicTest", "PointerTest")
  val fileNamesForProject8 = List("SimpleFunction")

  "main" should "produce the expected asm file" in {
    fileNamesForProject7.foreach{fileName =>
      val runCommands = Array(s"./src/test/resources/$fileName.vm")

      Main.main(runCommands)

      val actual = FileOps.readFile(s"./src/test/resources/$fileName.asm")
      val expected = FileOps.readFile(s"./src/test/resources/Expected$fileName.asm")
      actual shouldBe expected
    }
  }

  it should "produce the expected asm file for project 8" in {
    fileNamesForProject8.foreach{fileName =>
      val runCommands = Array(s"./src/test/resources-project-8/$fileName.vm")

      Main.main(runCommands)

      val actual = FileOps.readFile(s"./src/test/resources-project-8/$fileName.asm")
      val expected = FileOps.readFile(s"./src/test/resources-project-8/Expected$fileName.asm")
      actual shouldBe expected
    }
  }

  override def afterEach(): Unit = {
    fileNamesForProject7.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources/$fileName.asm")))
    fileNamesForProject7.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources-project-8/$fileName.asm")))
  }
}

