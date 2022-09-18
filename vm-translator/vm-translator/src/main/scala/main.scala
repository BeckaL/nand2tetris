import inputoutput.FileOps
import parser.Parser
import parser.Translator

import java.io.File
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

object Main {
  def main(args: Array[String]) =
    assemble(args.head)

  private def assemble(pathToVmFileOrDirectory: String): Unit = {
    if (pathToVmFileOrDirectory.endsWith(".vm")) {
      val filePath = pathToVmFileOrDirectory
      val filePathOut = filePath.replace(".vm", ".asm")
      val fileName = filePath.split("/").last.replace(".vm", "")
      val transformF: List[String] => List[String] = instructions => Translator.translate(Parser.parse(instructions), fileName)
      FileOps.readTransformAndWrite(inPath = filePath, outPath = filePathOut, transformF = transformF)
    } else if (Files.isDirectory(Paths.get(pathToVmFileOrDirectory))) {
      val pathToDirectory = Paths.get(pathToVmFileOrDirectory)
      val vmFiles = pathToDirectory.toFile.listFiles().toList.collect { case file if file.getPath.endsWith(".vm") => file.getPath }
      val asmFilePath = pathToDirectory.toString + "/" + pathToDirectory.toString.split("/").last + ".asm"
      Files.deleteIfExists(Paths.get(asmFilePath))
      vmFiles.foreach { filePath =>
        val fileName = filePath.split("/").last.replace(".vm", "")
        val transformF: List[String] => List[String] = instructions => Translator.translate(Parser.parse(instructions), fileName)
        FileOps.readTransformAndAppend(inPath = filePath, outPath = asmFilePath, transformF = transformF)
      }
    } else {
      throw new RuntimeException(s"failure: $pathToVmFileOrDirectory does not appear to be a vm file or a directory")
    }
  }
}
