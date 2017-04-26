//https://www.hackerrank.com/challenges/filter-elements

object Solution {

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines() drop(1)
  }
}

val list = List(1,2,1,5,1,1,2,15)

def amounts(list: List[Int]) = list.groupBy(identity).mapValues(_.length)
def pluralElements(list: List[Int], n: Int) = amounts(list).filter(t => t._2 >= n ).keys

pluralElements(list, 3)