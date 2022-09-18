package inputoutput

import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*

object FileOps {
  def readTransformAndWrite(inPath: String, outPath: String, transformF: List[String] => List[String]): Unit =
    val inputLines = readFile(inPath)
    val transformed = transformF(inputLines)
    writeFile(outPath, transformed)

  def readTransformAndAppend(inPath: String, outPath: String, transformF: List[String] => List[String]): Unit =
    val inputLines = readFile(inPath)
    val transformed = transformF(inputLines)
    writeFile(outPath, transformed, append = true)

  def readFile(path: String): List[String] = Files.readAllLines(Paths.get(path)).asScala.toList

  def writeFile(path: String, lines: List[String], append: Boolean = false): Unit =
    if (append) {
      Files.write(Paths.get(path), lines.mkString("\n").getBytes(), StandardOpenOption.APPEND)
    } else {
      Files.write(Paths.get(path), lines.mkString("\n").getBytes())
    }
}
