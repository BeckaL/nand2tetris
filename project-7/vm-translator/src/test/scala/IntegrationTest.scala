import inputoutput.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class IntegrationTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "main" should "produce the expected asm file" in {
    val runCommands = List("./src/test/resources", "SimpleAdd")

    Main.run(runCommands)

    val expected = FileOps.readFile("./src/test/resources/ExpectedSimpleAdd.asm")
    val actual = FileOps.readFile("./src/test/resources/SimpleAdd.asm")
    actual shouldBe expected
  }
}
