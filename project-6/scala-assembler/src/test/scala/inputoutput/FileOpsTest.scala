package inputoutput

import cats.effect.unsafe.implicits.global
import inputoutput.FileOps.{readFile, readTransformAndWrite, writeFile}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import java.nio.file.{Files, Paths}

class FileOpsTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterEach {
  val linesFromSampleFile = List(
    "//here",
    "are,",
    "@some",
    "   lines",
    "to read;"
  )

  val filePathToWriteTo = "./src/test/resources/out/ToWrite.hack"
  val filePathToReadFrom = "./src/test/resources/in/ToRead.asm"

  "readFile" should "read file lines" in {
    readFile(filePathToReadFrom).unsafeRunSync() shouldBe Right(linesFromSampleFile)
  }

  "writeToFile" should "write to a new file if it does not exist" in {
    (for {
      _ <- writeFile(filePathToWriteTo, linesFromSampleFile)
      linesAfterWrite <- readFile(filePathToWriteTo)
    } yield {
      linesAfterWrite shouldBe Right(linesFromSampleFile)
    }).unsafeRunSync()
  }

  it should "overwrite an existing file" in {
    val linesToOverwrite = List("some", "new", "lines")
    (for {
      _ <- writeFile(filePathToWriteTo, linesFromSampleFile)
      _ <- writeFile(filePathToWriteTo, linesToOverwrite)
      linesAfterWrite <- readFile(filePathToWriteTo)
    } yield {
      linesAfterWrite shouldBe Right(linesToOverwrite)
    }).unsafeRunSync()
  }

  "readTransformWrite" should "read lines, transform according to some function, and write to an output file" in {
    def transformFunction(in: List[String]): List[String] = in.map(_ => "blah")
    (for {
      _ <- readTransformAndWrite(inPath = filePathToReadFrom, outPath = filePathToWriteTo, transformF = transformFunction)
      lines <- readFile(filePathToWriteTo)
    } yield {
      lines shouldBe Right(transformFunction(linesFromSampleFile))
    }).unsafeRunSync()
  }

  override def afterEach(): Unit =
    Files.deleteIfExists(Paths.get(filePathToWriteTo))
}
