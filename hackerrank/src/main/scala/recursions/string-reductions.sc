//https://www.hackerrank.com/challenges/string-reductions
//https://www.hackerrank.com/challenges/remove-duplicates


object Solution {

  def reduceString(line: String): String = {

    def reduceStringAcc(line: List[Char], acc: List[Char], seenLetters: Set[Char]): List[Char] = line match {
      case Nil => acc.reverse
      case x::tail if seenLetters contains x => reduceStringAcc(tail, acc, seenLetters)
      case x::tail => reduceStringAcc(tail, x::acc, seenLetters + x)
    }

    reduceStringAcc(line.toList, List(), Set()).mkString
  }

  def main(args: Array[String]) {
    val line = io.Source.stdin.getLines()

    print(reduceString(line.next()))
  }
}

val testString1 = "pprrqq"
Solution.reduceString(testString1)
val testString2 = "accabb"
Solution.reduceString(testString2)