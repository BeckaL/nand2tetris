import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.implicits._
import java.nio.file.{Files, Paths}
import scala.io.Source

object FileOps {
  def readTransformAndWrite(inPath: String, outPath: String, transformF: List[String] => List[String]): IO[Either[String, Unit]] =
    (for {
      lines <- EitherT(readFile(inPath))
      transformed = transformF(lines)
      r <- EitherT.right[String](writeFile(outPath, transformed))
    } yield r).value

  def readFile(path: String): IO[Either[String, List[String]]] =
    Resource
      .fromAutoCloseable(IO {
        Source.fromFile(path)
      })
      .use(file => IO.pure(file.getLines().toList))
      .attempt
      .map(_.left.map(_.getMessage))


  def writeFile(path: String, lines: List[String]): IO[Unit] =
    val pathToFile = Paths.get(path)
    IO(Files.write(pathToFile, lines.mkString("\n").getBytes()))
}
