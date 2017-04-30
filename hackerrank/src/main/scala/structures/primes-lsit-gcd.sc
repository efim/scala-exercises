class Number(val decomposition: Map[Int, Int]) {

  def this(rawDecomposition: List[Int]) = this(Number.processRaw(rawDecomposition))

  override def toString: String = {
    decomposition.toList.map({ case (a, b) => List(a, b)})
      .sortBy(_.head).flatten.mkString(" ")
  }
}
object Number {
  def processRaw(rawDecomposition: List[Int]): Map[Int, Int] = {
    rawDecomposition.sliding(2, 2).map {
      case List(a, b) => a -> b
    }.toMap
  }

  def gcd(a: Number, b: Number): Number = {
    val sharedPrimes = a.decomposition.keySet.intersect(b.decomposition.keySet)

    val gcdDecomposition = sharedPrimes.map(key => {
      val aPower = a.decomposition.getOrElse(key, 0)
      val bPower = b.decomposition.getOrElse(key, 0)

      key -> (if (aPower < bPower) aPower else bPower)
    }).toMap

    new Number(gcdDecomposition)
  }

  def listGcd(numbers: List[Number]): Number = {
    numbers.foldLeft(numbers.head)(gcd)
  }
}

object Solution {

  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines()
    solve(input.toList)
  }

  def solve(input: List[String]): Unit = {
    val decompositions = input.drop(1).map(_.split(" ").map(_.toInt).toList)
//    println(decompositions)
    val numbers = decompositions.map(new Number(_))

    println(Number.listGcd(numbers))
  }
}

val testInput = "10\n2 5 3 1 5 4 7 3 13 1 19 1 23 2 31 1\n2 3 3 2 5 4 7 2 29 1\n2 5 3 1 5 4 7 2 11 3 13 1\n2 4 3 1 5 5 7 3 19 1 23 1 31 2\n2 4 3 4 5 4 7 2 13 1 23 1 29 1\n2 3 3 1 5 4 7 4 11 1 19 1\n2 4 3 1 5 4 7 2\n2 5 3 1 5 5 7 2 13 2 19 1 29 1\n2 3 3 1 5 6 7 2\n2 3 3 2 5 4 7 3 17 1 31 1"
val inputLines = testInput.split("\n").toList
Solution.solve(inputLines)