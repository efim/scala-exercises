import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val (pref, suf1, suf2) = extractPrefix(lines.head, lines.tail.head)
    printWithLen(pref)
    printWithLen(suf1)
    printWithLen(suf2)
  }

  def extractPrefix(str1: String, str2: String): (String, String, String) = {
    @tailrec
    def extractPrefixAcc(line1: List[Char], line2: List[Char], acc: List[Char]): (List[Char], List[Char], List[Char]) = {
      if (line1.headOption.isEmpty || line2.headOption.isEmpty) (acc.reverse, line1, line2)
      else if (line1.head == line2.head) extractPrefixAcc(line1.tail, line2.tail, line1.head::acc)
      else (acc.reverse, line1, line2)
    }

    val (prefList, str1List, str2List) = extractPrefixAcc(str1.toList, str2.toList, List())
    (prefList.mkString, str1List.mkString, str2List.mkString)
  }

  def printWithLen(str: String): Unit = println(str.length + " " + str)
}

//Solution.extractPrefix("aaab", "aaaab")
Solution.printWithLen("")

