import analyser.CompilationEngine.compileClass
import analyser.{DefaultTokeniser, InputUtils, LexicalElem, Tokeniser}
import inputoutput.FileOps.{readFile, writeFile}

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = args.head
    val fileLines = readFile(inputFile)
    val stripped = InputUtils.stripInput(fileLines)
    val outputFile = inputFile.replace(".jack", ".xml")
    val tokeniser = DefaultTokeniser(stripped.mkString(" "))
    tokeniser.advance()
    val tokens = compile(tokeniser, List())
    writeFile(outputFile, tokens.map(_.toTokenString))
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
