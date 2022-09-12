import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.effect.unsafe.implicits.global
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Files, Paths}

class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach {

  val fileNames = List("Max", "MaxL", "Pong", "PongL", "Rect", "RectL", "Sum1ToN")

  def assertProducedFileEqualsExpected(asmFileName: String, expectedFileName: String) = {
    val resourcePath = "./src/test/resources"
    (for {
      _ <- Main.run(List(resourcePath, asmFileName))
      expectedLines <- FileOps.readFile("./src/test/resources" + s"/out/$expectedFileName.hack")
      actualLines <- FileOps.readFile("./src/test/resources" + s"/out/$asmFileName.hack")
    } yield expectedLines shouldBe actualLines
      ).unsafeRunSync()
  }

  "main" should "convert a file to binary correctly and write to an out file" in {
    fileNames.foreach{filename =>
      assertProducedFileEqualsExpected(filename, s"Expected$filename")
    }
  }

  override def afterEach(): Unit =
    fileNames.foreach(fileName => Files.deleteIfExists(Paths.get(s"./src/test/resources/out/$fileName.hack")))
}
