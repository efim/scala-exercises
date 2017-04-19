object hackerrank {
  object Solution {
    def main(args: Array[String]) {
      def fact(n: Int): Int = if (n == 1) 1 else n * fact(n - 1)

      for (n <- io.Source.stdin.getLines) {
        print(fact(Integer.parseInt(n.toString)) + "\n")
      }
    }
  }

  val stdinString = "2\n3\n5"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)
}

// this one reads in "n" test cases then does a function for each one

object hackerrank2{
  def main(args: Array[String]) {
    def permute(s: String): String = if (s.isEmpty) "" else s.tail.head + s.head.toString + permute(s.tail.tail)

    val stdin = io.Source.stdin.getLines().toList
    val n = stdin(0).toInt
    for (i <- 1 to n) {
      println(permute(stdin(i)))
    }
  }

  val stdinString = "2\n3\n5"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  main(null)
}
