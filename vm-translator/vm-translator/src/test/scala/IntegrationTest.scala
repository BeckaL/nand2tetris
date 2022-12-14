import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import java.nio.file.{Files, Paths}
import org.scalatest.BeforeAndAfterEach


class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach{
  val fileNamesForProject7 = List("SimpleAdd", "StackTest", "StaticTest", "BasicTest", "PointerTest")
  val fileNamesForProject8 = List("SimpleFunction", "BasicLoop", "FibonacciSeries", "NestedCall")

  "main" should "produce the expected asm file" in {
    fileNamesForProject7.foreach{fileName =>
      val runCommands = Array(s"./src/test/resources/$fileName.vm", "false")

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

  it should "accept a directory and convert with a single vm file inside" in {
    val runCommands = Array("./src/test/resources-project-8/BasicLoopDirectory")

    Main.main(runCommands)

    val actual = FileOps.readFile(s"./src/test/resources-project-8/BasicLoopDirectory/BasicLoopDirectory.asm")
    val expected = FileOps.readFile(s"./src/test/resources-project-8/BasicLoopDirectory/ExpectedBasicLoop.asm")
    actual shouldBe expected
  }

  it should "be able to translate a directory with multiple files" in {
    val runCommands = Array("./src/test/resources-project-8/FibonacciElement")

    Main.main(runCommands)

    val actual = FileOps.readFile(s"./src/test/resources-project-8/FibonacciElement/FibonacciElement.asm")
    val expected = FileOps.readFile(s"./src/test/resources-project-8/FibonacciElement/ExpectedFibonacciElement.asm")

    actual shouldBe expected
  }

  it should "work for statics" in {
    val runCommands = Array("./src/test/resources-project-8/StaticsTest")

    Main.main(runCommands)

    val actual = FileOps.readFile(s"./src/test/resources-project-8/StaticsTest/StaticsTest.asm")
    val expected = FileOps.readFile(s"./src/test/resources-project-8/StaticsTest/ExpectedStaticsTest.asm")

    actual shouldBe expected
  }

  override def afterEach(): Unit = {
    fileNamesForProject7.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources/$fileName.asm")))
    fileNamesForProject8.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources-project-8/$fileName.asm")))
    Files.deleteIfExists(Paths.get("./src/test/resources-project-8/BasicLoopDirectory/BasicLoopDirectory.asm"))
    Files.deleteIfExists(Paths.get("./src/test/resources-project-8/FibonacciElement/FibonacciElement.asm"))
  }
}

