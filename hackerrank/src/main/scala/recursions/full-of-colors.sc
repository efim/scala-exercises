import scala.annotation.tailrec
//https://www.hackerrank.com/challenges/sequence-full-of-colors

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines() drop 1
    lines.foreach(line => println(if (isFullOfColor(line.toList)) "True" else "False"))
  }

  def isFullOfColor(line: List[Char]): Boolean = {
    @tailrec
    def isFullOfColorAcc(line: List[Char], rgDiff: Int, ybDiff: Int): Boolean = {
      if (scala.math.abs(rgDiff) > 1 || scala.math.abs(ybDiff) > 1) false
      else {
        def diffUpdated(first:Char, second:Char)(c: Char, diff:Int): Int =
          if (c == first) diff + 1
          else if (c == second) diff - 1
          else diff

        def rgUpdated = diffUpdated('R', 'G')_
        def ybUpdated = diffUpdated('Y', 'B')_

        line match {
          case c::tail => isFullOfColorAcc(tail, rgUpdated(c, rgDiff), ybUpdated(c, ybDiff))
          case _ if rgDiff == 0 && ybDiff == 0 => true
          case _ => false
        }
      }
    }

    isFullOfColorAcc(line, 0, 0)
  }
}

def isFullOfColor(line: List[Char]): Boolean = {
  def isFullOfColorAcc(line: List[Char], rgDiff: Int, ybDiff: Int): Boolean = {
    if (scala.math.abs(rgDiff) > 1 || scala.math.abs(ybDiff) > 1) false
    else {
      def diffUpdated(first:Char, second:Char)(c: Char, diff:Int): Int =
        if (c == first) diff + 1
        else if (c == second) diff - 1
        else diff

      def rgUpdated = diffUpdated('R', 'G')_
      def ybUpdated = diffUpdated('Y', 'B')_

      line match {
        case c::tail => isFullOfColorAcc(tail, rgUpdated(c, rgDiff), ybUpdated(c, ybDiff))
        case _ if rgDiff == 0 && ybDiff == 0 => true
        case _ => false
      }
    }
  }

  isFullOfColorAcc(line, 0, 0)
}


Solution.isFullOfColor("RGGR".toList)
Solution.isFullOfColor("RYBG".toList)
Solution.isFullOfColor("RYRB".toList)
Solution.isFullOfColor("YGYGRBRB".toList)