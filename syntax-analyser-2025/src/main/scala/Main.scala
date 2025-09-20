import analyser.CompilationEngine.compileClass
import analyser.{DefaultTokeniser, InputUtils, LexicalElem, Tokeniser}
import inputoutput.FileOps.{readFile, writeFile}
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    m(args)
  }

  def m(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: JackCompiler <file.jack | directory>")
      sys.exit(1)
    }

    val input = new File(args.head)

    if (!input.exists()) {
      println(s"Error: ${input.getPath} does not exist.")
      sys.exit(1)
    }

    val jackFiles: Seq[File] =
      if (input.isDirectory) {
        input.listFiles().filter(f => f.isFile && f.getName.endsWith(".jack"))
      } else if (input.isFile && input.getName.endsWith(".jack")) {
        Seq(input)
      } else {
        println("Error: input must be a .jack file or a directory containing .jack files.")
        sys.exit(1)
        Seq.empty
      }

    jackFiles.foreach { file =>
      val fileLines = readFile(file.getPath)
      val stripped = InputUtils.stripInput(fileLines)
      val outputFile = file.getPath.stripSuffix(".jack") + ".xml"
      val tokeniser = DefaultTokeniser(stripped.mkString(" "))
      tokeniser.advance()
      val tokens = compile(tokeniser, List())
      writeFile(outputFile, tokens.map(_.toTokenString))
      println(s"Compiled ${file.getName} -> ${outputFile}")
    }
  }

  //TODO make work on a directory

  private def compile(tokeniser: Tokeniser, statements: List[LexicalElem]): List[LexicalElem] = {
    if (tokeniser.currentToken == "class") {
      compileClass(tokeniser) match {
        case Right(t) => t
        case Left(err) => throw new RuntimeException(err)
      }
    } else {
      throw new RuntimeException("expected class")
    }
  }
}
