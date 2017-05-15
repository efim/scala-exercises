def expand_from(beginning: Stream[Boolean]): Stream[Boolean] = {
  val inverted = beginning.map(bool => !bool).toStream
  inverted #::: expand_from(beginning #::: inverted)
}

def sequence(list: List[Boolean]): Stream[Int] = {
  val beginning = list.toStream
  val result = beginning.toStream #::: expand_from(beginning)
  result.map(if (_) 1 else 0)
}

val taskSequence = sequence(List(false)).take(30).mkString(" ")


object Solution {

  def main(args: Array[String]) {

    val taskSequence = sequence(List(false))

    val queries = (io.Source.stdin.getLines() drop 1).map(_.toInt)
    // myIter.drop(2).take(1).toList.headOption
    for (q <- queries) {println(taskSequence.slice(q,q+1).mkString(""))}

  }
}
