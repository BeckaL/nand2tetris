package inputoutput

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

object FileOps {
  def readTransformAndWrite(inPath: String, outPath: String, transformF: List[String] => List[String]): Unit =
    val inputLines = readFile(inPath)
    val transformed = transformF(inputLines)
    writeFile(outPath, transformed)

  def readFile(path: String): List[String] = Files.readAllLines(Paths.get(path)).asScala.toList

  def writeFile(path: String, lines: List[String]): Unit = Files.write(Paths.get(path), lines.mkString("\n").getBytes())
}
