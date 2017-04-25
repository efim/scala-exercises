import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]) {
      val string = io.Source.stdin.getLines().foreach(s => println(compress(s)))
  }

  def compress(string: String):String = {

    @tailrec
    def breakdown(string: String, acc: List[String]): List[String] = string match {
      case "" => acc.reverse
      case s => {
        val (head, tail) = s span (_ == s.head)
        breakdown(tail, head::acc)
      }
    }

    breakdown(string, List()).map(s => if (s.length == 1) s else s.head+s.length.toString).mkString

  }
}

Solution.compress("aaaaaabbbaccdeee")

//val (head, tail ) = "aaaaab" span (ch => ch.equals('a'))

/*
val s = "aaaa"
s.head + s.length.toString*/
