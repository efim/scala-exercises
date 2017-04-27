//https://www.hackerrank.com/challenges/sequence-full-of-colors

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines() drop 1

  }
}

def isFullOfColor(line: List[Char]): Boolean = {
  def isFullOfColorAcc(line: List[Char], rgDiff: Int, ybDiff: Int): Boolean = {
    if (scala.math.abs(rgDiff) > 1 || scala.math.abs(ybDiff) > 1) false
    else {
      def diffUpdated(first:Char, second:Char)(c: Char, diff:Int): Int =
        if (c == first) diff + 1
        else diff - 1

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


isFullOfColor("RGGR".toList)
isFullOfColor("RYBG".toList)
isFullOfColor("RYRB".toList)
isFullOfColor("YGYGRBRB".toList)