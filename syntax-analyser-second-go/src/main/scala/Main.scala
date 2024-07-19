import analyser.CompilationEngine.{compileDo, compileLet, compileIf}
import analyser.{DefaultTokeniser, InputUtils, LexicalElement, Tokeniser}
import inputoutput.FileOps.*

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = args.head
    val fileLines = readFile(inputFile)
    val stripped = InputUtils.stripInput(fileLines)
    val outputFile = inputFile.replace(".jack", ".xml")
    val tokeniser = DefaultTokeniser(stripped.mkString(" "))
    tokeniser.advance()
    val tokens = compile(tokeniser, List())
    writeFile(outputFile, "<tokens>" +: tokens.map(_.toTokenString) :+ "</tokens>")
  }

  @tailrec
  private def compile(tokeniser: Tokeniser, statements: List[LexicalElement]): List[LexicalElement] = {
    val newStatements = tokeniser.currentToken match {
      case "let" => compileLet(tokeniser).map(s => statements ++ s)
      case "do" => compileDo(tokeniser).map(s => statements ++ s)
      case "if" => compileIf(tokeniser).map(s => statements ++ s)
      case otherToken => throw new RuntimeException(s"uh oh $otherToken")
    }
    newStatements match {
      case Right(newStatements) => if (tokeniser.hasMoreTokens)
        compile(tokeniser, newStatements)
      else
        newStatements
      case Left(err) => throw new RuntimeException(err)
    }
  }
}
