import assembler.Assembler
import cats.effect.ExitCode.Error
import cats.effect.{ExitCode, IO, IOApp}
import inputoutput.FileOps

object Main extends IOApp {
  def run(args: List[String]) =
    assemble(args.head, args(1))

  private def assemble(pathToResourcesFolder: String, asmFileName: String): IO[ExitCode] =
    val filePathIn = pathToResourcesFolder + "/in/" + asmFileName + ".asm"
    val filePathOut = pathToResourcesFolder + "/out/" + asmFileName + ".hack"
    FileOps.readTransformAndWrite(inPath = filePathIn, outPath = filePathOut, transformF = Assembler.assemble).flatMap{
      case Right(()) => IO.pure(println("success!")).as(ExitCode.Success)
      case Left(message) => IO.pure(println(s"assembling failed: $message")).as(ExitCode.Error)
    }
}

