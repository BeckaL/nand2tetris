import inputoutput.FileOps.{readFile, writeFile}

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = args.head
    val fileLines = readFile(inputFile)
    val outputFile = inputFile.replace(".jack", ".xml")
    writeFile(outputFile, List("implement me"))
  }
}
