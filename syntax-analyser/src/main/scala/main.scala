import inputoutput.FileOps.*

object Main {
  def main(args: Array[String]) = {
    val inputFile = args.head
    val outputFile = inputFile.replace(".jack", ".xml")
    writeFile(outputFile, List(""))
  }
}
