//https://www.hackerrank.com/challenges/filter-elements

object Solution {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines() drop(1)
    lines.sliding(2, 2).foreach(inputPair => {
      val delimiter = inputPair.head.split(" ").tail.head.toInt
      val numbers = inputPair.tail
        .head.split(" ").map(_.toInt).toList

      val filtered = pluralElements(numbers, delimiter).toList

      def sortedFiltered(items: List[Int], required: List[Int], seen: Set[Int], acc: List[Int]): List[Int] = items match {
        case Nil => acc.reverse
        case x::tail => sortedFiltered(tail, required, seen + x, if (required.contains(x) && !seen.contains(x)) x::acc else acc)
      }

      println(if (!filtered.isEmpty) sortedFiltered(numbers, filtered, Set(), List()).mkString(" ") else "-1")
    })
  }

  def amounts(list: List[Int]) = list.groupBy(identity).mapValues(_.length)
  def pluralElements(list: List[Int], n: Int) = amounts(list).filter(t => t._2 >= n ).keys

}

val list = List(1,2,1,5,1,1,2,15)


Solution.pluralElements(list, 3)