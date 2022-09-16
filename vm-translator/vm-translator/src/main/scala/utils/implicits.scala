package utils

// TODO: this is copied from project 6, work out how to share and make in makefile
object implicits {
  implicit class StringOps(string: String) {
    def splitIntoMaxTwo(splitString: String): (String, Option[String]) =
      string.indexOf(splitString) match {
        case -1 => (string.trim, None)
        case n =>
          val (first, second) = string.splitAt(n)
          (first.trim, Some(second.drop(1).trim))
      }

    def toPaddedBinaryString(requiredSize: Int): String =
      string.toInt.toPaddedBinaryString(requiredSize)

    def stripCommentsAndWhitespace: String = splitIntoMaxTwo("//")._1
  }

  implicit class IntOps(i: Int) {
    def toPaddedBinaryString(requiredSize: Int) = {
      val unpaddedBinary = i.toBinaryString
      "0" * (requiredSize - unpaddedBinary.length) + unpaddedBinary
    }
  }
}
