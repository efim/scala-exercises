
class Sierpinski(val level: Int, val rowsNum: Int, val colsNum: Int) {

  def this(level: Int) {
    this(level, 32, 63)
  }

  class Line(val rowNum: Int, val items: List[Boolean])
  lazy val rows: List[Line] =
    if (level == 0) (1 to rowsNum).map(getLevelZeroRow).toList
    else getLvlNLines(level)

  def getLevelZeroRow(row:Int):Line = {
    val colMiddle = colsNum / 2 + colsNum % 2
    def getBool(col:Int):Boolean = math.abs(col - colMiddle) < row

    new Line(row, (1 to colsNum).map(getBool).toList)
  }

  override def toString(): String =
    rows.map(_.items.map((cell: Boolean) => if (cell) '1' else '_')).map(_.mkString).mkString("\n")

  def fractions: Stream[IndexedSeq[(Int, Int)]] = {
    def fractionsN(n: Int):IndexedSeq[(Int,Int)] = {
      val div = math.pow(2,n).toInt
      for {
        mul <- 1 to div - 1
      } yield (mul,div)
    }

    def fractionsFrom(n: Int): Stream[IndexedSeq[(Int, Int)]] = Stream.cons(fractionsN(n), fractionsFrom(n+1))

    fractionsFrom(1)
  }

  def getLvlNLines(n:Int): List[Line] = {

    def lineNumToMirror(mul:Int, div:Int, line:Int) = {
      val low_middle = rowsNum/div * mul - 1
      val high_middle = low_middle + 1
      val diff = low_middle - line
      high_middle + diff
    }

//    def lineMirrored(li)

    def mirrorMatrix(matrix:List[List[Boolean]], fraction:(Int,Int)):List[List[Boolean]] =
      ???

    ???
  }
}

print(new Sierpinski(0))

