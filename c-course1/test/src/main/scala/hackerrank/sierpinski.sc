
class Sierpinski(level: Int, rowsNum: Int, colsNum: Int) {

  object Line {
    def getLevelZeroRow(row:Int): List[Boolean] = {
      val colMiddle = colsNum / 2 + colsNum % 2
      def getBool(col:Int):Boolean = math.abs(col - colMiddle) < row

      (1 to colsNum).map(getBool).toList
    }
  }
  class Line(val rowNum: Int, val items: List[Boolean]) {
    def this(rowNum: Int) = this(rowNum, Line.getLevelZeroRow(rowNum))

    def combine(other: Option[Line]): Line = other match {
      case None => this
      case Some(line) => new Line(rowNum, (items zip line.items).map(pair => pair._1 && !pair._2))
    }
  }

  def this(level: Int) {
    this(level, 32, 63)
  }

  override def toString(): String =
    rows.map(_.items.map((cell: Boolean) => if (cell) '1' else '_')).map(_.mkString).mkString("\n")

  lazy val rows: List[Line] =
    if (level == 0) getLvl0Lines
    else getLvlNLines(level)

  def getLvl0Lines = (1 to rowsNum).map(new Line(_)).toList

  def getLvlNLines(n:Int): List[Line] = {

    class Fraction(val multiplier: Int, val divisor: Int)
    lazy val fractions: Stream[Fraction] = {
      def fractionsN(n: Int):IndexedSeq[Fraction] = {
        def isSimpleFraction(mul: Int, div: Int): Boolean = {
          def gcd(a: Int,b: Int): Int = {
            if(b ==0) a else gcd(b, a%b)
          }

          gcd(mul, div) == 1
        }

        val div = math.pow(2,n).toInt
        for {
          mul <- 1 to div - 1 if isSimpleFraction(mul, div)
        } yield new Fraction(mul,div)
      }

      def fractionsFrom(n: Int): Stream[IndexedSeq[Fraction]] = Stream.cons(fractionsN(n), fractionsFrom(n+1))

      fractionsFrom(1) take level flatten
    }

    def lineNumToMirror(mul:Int, div:Int, line:Int) = {
      val low_middle = rowsNum/div * mul - 1
      val high_middle = low_middle + 1
      val diff = low_middle - line
      high_middle + diff
    }

    def mirrorLines(rows: List[Line], fraction: Fraction): List[Line] = {
      val lowerLinesAmount = rowsNum * fraction.multiplier / fraction.divisor
      val higherLinesAmount = rowsNum - lowerLinesAmount
      val lineFraction = rowsNum / fraction.divisor
      //can be done with sliding all at once, but would require bigger refactoring.

      val (topRows, lowerRows) = rows splitAt higherLinesAmount
      val topRowsToApply: Stream[Option[Line]] = topRows.reverse.take(lineFraction).map(Some(_)).toStream append Stream.continually(None)

      val lowerRowsUpdated = (lowerRows zip topRowsToApply).map(pair => pair._1.combine(pair._2))

      topRows ::: lowerRowsUpdated
    }

    fractions.foldLeft(getLvl0Lines)(mirrorLines(_,_))
  }
}

/*print(new Sierpinski(1))
print(new Sierpinski(2))
print(new Sierpinski(3))
print(new Sierpinski(4))
print(new Sierpinski(5))*/

//So, ok. I had an error in main idea behind the algo.
//I shouldn't just fold over whole top rows, but only some  small part of them.
//Possibly just the adjaicent part.

object Solution {
  def drawTriangles(n: Int) {
    //Draw the N'th iteration of the fractal as described
    // in the problem statement
    print(new Sierpinski(n))
  }

  def main(args: Array[String]) {
    drawTriangles(readInt())
  }
}


