import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.effect.unsafe.implicits.global

class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "main" should "produce the expected asm file" in {
    val runCommands = List("./src/test/resources", "SimpleAdd")

    Main.run(runCommands).unsafeRunSync()

    val expected = FileOps.readFile("./src/test/resources/ExpectedSimpleAdd.asm").unsafeRunSync().getOrElse(throw new RuntimeException("failed 1"))
    val actual = FileOps.readFile("./src/test/resources/SimpleAdd.asm").unsafeRunSync().getOrElse(throw new RuntimeException("failed 2"))
    actual shouldBe expected
  }
}
