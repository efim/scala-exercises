val (stepPoints, start, end) = (9, 0, 2)

val steps = Range(0, stepPoints - 1)
val (r0, g, seed, p) = (1, 3, 4, 7)

def getShifted(pos: Int, shift: Int): Int = {
  val shifted = (pos + shift) % stepPoints
  if (shifted >= 0) shifted else shifted + stepPoints
}

def reach(pos: Int): Int = {
  if (pos==0) 1
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

  val visitedExtended = visited union newPaths.map(_.head).toSet
//  println("new paths: " + newPaths)
//  println("visited extended: " + visitedExtended)
  paths append pathsFrom(newPaths, visitedExtended)
}

val solution = pathsFrom(Stream(initialPath), Set(start))

solution.filter(isWinning).take(1).toList
