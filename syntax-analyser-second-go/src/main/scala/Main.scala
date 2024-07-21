import analyser.CompilationEngine.{compileClass, compileDo, compileIf, compileLet, compileStatement}
import analyser.{DefaultTokeniser, InputUtils, LexicalElem, Tokeniser}
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
  private def compile(tokeniser: Tokeniser, statements: List[LexicalElem]): List[LexicalElem] = {
    if (tokeniser.currentToken == "class") {
      compileClass(tokeniser) match {
        case Right(t) => t
        case Left(err) => throw new RuntimeException(err)
      }
    } else { //TODO legacy everything needs to be in a class?
      val newStatements = compileStatement(tokeniser).map(s => statements ++ s)
      newStatements match {
        case Right(newStatements) => if (tokeniser.hasMoreTokens)
          compile(tokeniser, newStatements)
        else
          newStatements
        case Left(err) => throw new RuntimeException(err)
      }
    }
  }
}
