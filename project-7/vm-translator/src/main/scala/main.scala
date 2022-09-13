import cats.effect.ExitCode.Error
import cats.effect.{ExitCode, IO, IOApp}
import inputoutput.FileOps
import parser.Parser
import parser.Translator

import scala.util.{Failure, Success, Try}

object Main extends IOApp {
  def run(args: List[String]) =
    assemble(args.head, args(1))

  private def assemble(pathToResourcesFolder: String, vmFilename: String): IO[ExitCode] =
    val filePathIn = pathToResourcesFolder + "/"  + vmFilename + ".vm"
    val filePathOut = pathToResourcesFolder + "/" + vmFilename + ".asm"
    val transformF: List[String] => List[String] = instructions => Translator.translate(Parser.parse(instructions))
    Try(FileOps.readTransformAndWrite(inPath = filePathIn, outPath = filePathOut, transformF = transformF)) match {
      case Success(()) => IO.pure(println("success!")).as(ExitCode.Success)
      case Failure(exception) => IO.pure(println(s"assembling failed: ${exception.getMessage}")).as(ExitCode.Error)
    }
}
