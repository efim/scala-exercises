val damage = 8
val time = 6

val monsters: Stream[Int] = "16 19 7 11 23 8 16".split(" ")
  .map(_.toInt).toStream.sorted

monsters.toList

val shotsToKill:Stream[Int] = monsters.
  map(health => health / damage + scala.math.min(1, health % damage))

shotsToKill.toList

def prefixSums(in: Stream[Int]): Stream[Int] = {
  def prefixSumsInner(in: Stream[Int], add: Int): Stream[Int] = in match {
    case Stream.Empty => Stream.empty
    case head #:: tail => {
      val sumHead = head + add
      sumHead #:: prefixSumsInner(tail, sumHead)
    }
  }

  prefixSumsInner(in, 0)
}

val shotsTillDead:Stream[Int] = prefixSums(shotsToKill)

shotsTillDead.toList

shotsTillDead.takeWhile(_ <= time).length