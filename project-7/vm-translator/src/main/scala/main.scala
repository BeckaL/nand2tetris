import inputoutput.FileOps
import parser.Parser
import parser.Translator

import scala.util.{Failure, Success, Try}

object Main {
  def main(args: Array[String]) =
    assemble(args.head, args(1))

  private def assemble(pathToResourcesFolder: String, vmFilename: String): Unit =
    val filePathIn = pathToResourcesFolder + "/"  + vmFilename + ".vm"
    val filePathOut = pathToResourcesFolder + "/" + vmFilename + ".asm"
    val transformF: List[String] => List[String] = instructions => Translator.translate(Parser.parse(instructions), vmFilename)
    Try(FileOps.readTransformAndWrite(inPath = filePathIn, outPath = filePathOut, transformF = transformF)) match {
      case Success(()) => println("success!")
      case Failure(exception) => println(s"assembling failed: ${exception.getMessage}")
    }
}
