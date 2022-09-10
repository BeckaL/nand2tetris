import cats.effect.ExitCode.Error
import cats.effect.{ExitCode, IO, IOApp}


object Main extends IOApp {
  def run(args: List[String]) =
    assemble(args.head, args(1))

  private def assemble(pathToResourcesFolder: String, asmFileName: String): IO[ExitCode] =
    val filePathIn = pathToResourcesFolder + "/in/" + asmFileName + ".asm"
    val filePathOut = pathToResourcesFolder + "/out/" + asmFileName + ".hack"
    //TODO fix
    def transformF(in: List[String]): List[String] = in.map(_ => "blah")
    FileOps.readTransformAndWrite(inPath = filePathIn, outPath = filePathOut, transformF = transformF).flatMap{
      case Right(()) => IO.pure(println("success!")).as(ExitCode.Success)
      case Left(message) => IO.pure(println(s"assembling failed: $message")).as(ExitCode.Error)
    }
}

