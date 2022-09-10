import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.effect.unsafe.implicits.global
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Files, Paths}

class IntegrationTest  extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach {
  val expectedCreatedHackFile = "./src/test/resources/out/Sum1ToN.hack"

  //FIXME
  "assemble" should "do the right thing" in {
    val resourcePath = "./src/test/resources"
    val asmFileName = "Sum1ToN"

    (for {
      _ <- Main.run(List(resourcePath, asmFileName))
      expectedLines <- FileOps.readFile("./src/test/resources" + "/out/ExpectedSum1ToN.hack")
      actualLines <- FileOps.readFile("./src/test/resources" + "/out/Sum1ToN.hack")
    } yield {
      expectedLines shouldBe actualLines
    }).unsafeRunSync()
  }

  override def afterEach(): Unit =
    Files.deleteIfExists(Paths.get(expectedCreatedHackFile))
}
