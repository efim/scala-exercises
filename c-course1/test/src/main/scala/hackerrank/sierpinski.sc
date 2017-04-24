import com.sun.xml.internal.fastinfoset.algorithm.BooleanEncodingAlgorithm

val rows = 32
val cols = 63


def printMatrix(matrix: List[List[Boolean]]):Unit =
  matrix.map(_.map((cell:Boolean) => if (cell) '1' else '_')).map(_.mkString).foreach(println)


def getInitialRow(row:Int):List[Boolean] = {
  val middle = cols / 2 + cols % 2
  def getBool(col:Int):Boolean = math.abs(col - middle) < row

  (1 to cols).map(getBool).toList
}

def getLvl0() = (1 to rows).map(getInitialRow).toList



def getLvln(n:Int): List[List[Boolean]] = {
  def fractions:IndexedSeq[(Int,Int)] = {
    for {
      div <- for {i <- 1 to n+1} yield math.pow(2,i).toInt
      mul <- 1 to div - 1
    } yield (mul,div)
  }

  def lineNumToMirror(mul:Int, div:Int, line:Int) = {
    val low_middle = rows/div * mul - 1
    val high_middle = low_middle + 1
    val diff = low_middle - line
    high_middle + diff
  }

  def mirrorMatrix(matrix:List[List[Boolean]], fraction:(Int,Int)):List[List[Boolean]] =
    ???

  ???
}

printMatrix(getLvl0())

