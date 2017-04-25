val testLines1 = List("0 0", "0 1", "1 1", "1 0")

object Solution {

  def parseLine(line: String) = {
    val res = line.split("\\s+")
      .map(_.toDouble)
      .toList
    res
  }

  def main(args: Array[String]) {
    //    println("main is executing")
    val lines = io.Source.stdin.getLines().toList drop 1

    print(calculateArea(lines))
  }

  def calculateArea(points: List[String]): Double = {
    val vertices = (points ::: List(points.head) map parseLine).sliding(2)

    val verticeToTerm = (pair: List[List[Double]]) => {

      val powTwo = (x:Double) => scala.math.pow(x, 2)
      //TODO make some OK data type for this
      val dx = pair.head.head*pair.tail.head.tail.head //x1*y2
      val dy = pair.head.tail.head*pair.tail.head.head //y1*x2
//      println("dx = " + dx + " dy = " + dy)
      val len = dx - dy
//      println(len)

      len
    }

    scala.math.abs(vertices.map(verticeToTerm).sum / 2)
  }
}

Solution.calculateArea(testLines1)