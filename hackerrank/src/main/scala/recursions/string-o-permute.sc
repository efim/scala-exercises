object Solution {

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines() drop 1
    val permuted = lines.map(line => permuteLine(line.toList))
    permuted.foreach(list => println(list.mkString))
  }

  def permuteLine(line: List[Char]): List[Char] = {
    def permuteLineAcc(line: List[Char], acc: List[Char]): List[Char] = line match {
      case x::y::tail => y::x::permuteLineAcc(tail, acc)
      case Nil => acc
      case _ => List()
    }

    permuteLineAcc(line, List())
  }
}

println(Solution.permuteLine("abcdpqrs".toList))