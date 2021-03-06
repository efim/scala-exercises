val (stepPoints, start, end) = (9, 0, 2)

val steps = Range(0, stepPoints - 1)
val (r0, g, seed, p) = (1, 3, 4, 7)

def getShifted(pos: Int, shift: Int): Int = {
  val shifted = (pos + shift) % stepPoints
  if (shifted >= 0) shifted else shifted + stepPoints
}

def reach(pos: Int): Int = {
  if (pos==0) r0
  else (getShifted(pos, -1) * g + seed) % p
}

def possibleEnds(pos: Int) = {
  val width = reach(pos)
  val moves = Range(-width, width + 1)

  for {
    move <- moves
  } yield getShifted(pos, move)
}

def possibleNewEnds(pos: Int, visited: Set[Int]) = {
  possibleEnds(pos).filterNot(visited.contains)
}

val initialPath = List(start)
def isWinning(path: List[Int]) =
  path.headOption.getOrElse(-1) == end

/*val paths = Stream(initialPath)
val newPaths = for {
  path <- paths
  end <- possibleNewEnds(path.head, Set(0))
} yield end :: path
newPaths.toList*/

//List of all possible new paths from current
def pathsFrom(paths: Stream[List[Int]], visited: Set[Int]): Stream[List[Int]] = {
  val newPaths = for {
    path <- paths
    end <- possibleNewEnds(path.head, visited)
  } yield end :: path

  val newPathsFiltered = newPaths.groupBy(_.head).values.map(_.head)

  val visitedExtended = visited union newPathsFiltered.map(_.head).toSet

  paths append pathsFrom(newPathsFiltered.toStream, visitedExtended)
}

val solution = pathsFrom(Stream(initialPath), Set(start))

solution.filter(isWinning).take(1).toList







///////

















object Solution {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().map(_.split(" ").map(_.toInt))

    val List(stepPoints, start, end) = lines.next().toList
    val steps = Range(0, stepPoints - 1)

    val List(r0, g, seed, p) = lines.next().toList

    def getShifted(pos: Int, shift: Int): Int = {
      val shifted = (pos + shift) % stepPoints
      if (shifted >= 0) shifted else shifted + stepPoints
    }

    def reach(pos: Int): Int = {
      if (pos==0) r0
      else (getShifted(pos, -1) * g + seed) % p
    }

    def possibleEnds(pos: Int) = {
      val width = reach(pos)
      val moves = Range(-width, width + 1)

      for {
        move <- moves
      } yield getShifted(pos, move)
    }

    def possibleNewEnds(pos: Int, visited: Set[Int]) = {
      possibleEnds(pos).filterNot(visited.contains)
    }

    val initialPath = List(start)
    def isWinning(path: List[Int]) =
      path.headOption.getOrElse(-1) == end

    //List of all possible new paths from current
    def pathsFrom(paths: Stream[List[Int]], visited: Set[Int]): Stream[List[Int]] = {
      val newPaths = for {
        path <- paths
        end <- possibleNewEnds(path.head, visited)
      } yield end :: path

      val newPathsFiltered = newPaths.groupBy(_.head).values.map(_.head)

      val visitedExtended = visited union newPathsFiltered.map(_.head).toSet

      paths append pathsFrom(newPathsFiltered.toStream, visitedExtended)
    }

    val solution = pathsFrom(Stream(initialPath), Set(start)).filter(isWinning)

    println(solution.headOption.getOrElse(List(-1)).head)


  }
}
