//https://www.hackerrank.com/challenges/rotate-string



object Solution {

  def rotations(line: String) = {
    for ( index <- 1 to line.length )
      yield line.drop(index)+line.take(index)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines() drop 1
    lines.foreach(string => println(rotations(string).mkString(" ")))
  }
}

print(Solution.rotations("abc").mkString(" "))
//assert("bca cab abc".split(" ") == rotations("abc"))
//assert("bcdea cdeab deabc eabcd abcde".split(" ") == rotations("abcde"))
//assert("z".split(" ") == rotations("z"))