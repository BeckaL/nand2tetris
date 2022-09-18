import inputoutput.FileOps
import parser.Parser
import parser.Translator

import scala.util.{Failure, Success, Try}

object Main {
  def main(args: Array[String]) =
    assemble(args.head)

  private def assemble(pathToVmFile: String): Unit = {
    val filePathOut = pathToVmFile.replace(".vm", ".asm")
    val fileName = pathToVmFile.split("/").last.replace(".vm", "")
    val transformF: List[String] => List[String] = instructions => Translator.translate(Parser.parse(instructions), fileName)
    FileOps.readTransformAndWrite(inPath = pathToVmFile, outPath = filePathOut, transformF = transformF)
  }
}
