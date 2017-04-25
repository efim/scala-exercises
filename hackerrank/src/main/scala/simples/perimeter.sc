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

    print(calculatePerimeter(lines))
  }

  def calculatePerimeter(points: List[String]): Double = {
    val vertices = (points:::List(points.head) map parseLine).sliding(2)

    val verticeToLen = (pair: List[List[Double]]) => {

      val powTwo = (x:Double) => scala.math.pow(x, 2)
      //TODO make some OK data type for this
      val dx = pair.head.head - pair.tail.head.head
      val dy = pair.head.tail.head - pair.tail.head.tail.head
      println("dx = " + dx + " dy = " + dy)
      val len = scala.math.sqrt(powTwo(dx) + powTwo(dy))
      println(len)

      len
    }

    vertices.map(verticeToLen).sum
  }

}
/*
val testLines1 = List("0 0", "0 1", "1 1", "1 0")
Solution.calculatePerimeter(testLines1)
*/
/*val testLines2 = List("0 0", "0 1", "1 1")
Solution.calculatePerimeter(testLines2)*/
/*val testLines3 = List("0 0", "0 2", "2 2", "2 0")
Solution.calculatePerimeter(testLines3)*/

val testLines4 = List("1043 770", "551 990", "681 463")
Solution.calculatePerimeter(testLines4)

