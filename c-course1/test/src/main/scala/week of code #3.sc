/*
//game of 1s and 0s
//list possible moves from current position: removing one element that is surrounded by 0s
def possibleMoves(list: List[Int]): List[List[Int]] = {
  def checkIfValid(triple: List[Int]): Boolean = triple.head == 0 && triple.last == 0

  val innerChecked = list.sliding(3).map(checkIfValid(_)).toList
  val indexedChecks = innerChecked.zipWithIndex.filter(_._1)
  //  val indexedChecks = innerChecked.zipWithIndex.map((pair: (Boolean, Int)) => (pair._1, pair._2 + 1)).filter(_._1)

  indexedChecks.map((pair: (Boolean, Int)) => list.patch(pair._2, Nil, 1)).distinct
  //  indexedChecks.map((pair: (Boolean, Int)) => list.take(pair._2) ::: list.drop(pair._2 + 1))
  //  indexedChecks.map((pair: (Boolean, Int)) => list.take(pair._2) ++ list.drop(pair._2 + 1))
}
val list = List(1, 0, 0, 1, 0, 0, 0)
possibleMoves(list)
possibleMoves(List(1))
possibleMoves(List(0, 0, 0, 0, 1, 0))


//returns true if player will win from this position with perfect game of both.
def isWinningPosition(list: List[Int]): Boolean = possibleMoves(list) match {
  //no possible moves
  case List() => false
  // exists at least one move that is loosing for the next player
  case moves if !moves.forall(isWinningPosition(_)) => true
  // all moves for other player are winning, we loose whatever we choose now
  case _ => false
}
assert(!isWinningPosition(List(1, 0, 0, 1)))
assert(isWinningPosition(List(1, 0, 1, 0, 1)))

//assert(!isWinningPosition(List(0, 0, 0, 0, 0, 0)))


//now, we just get if the initial list is winning.
//But I also need caching! So I would recalculate too much same evaluations.
/*case class Memo[A,B](f: A => B) extends (A => B) {
  private val cache = mutable.Map.empty[A, B]
  def apply(x: A) = cache getOrElseUpdate (x, f(x))
}

val fib: Memo[Int, BigInt] = Memo {
  case 0 => 0
  case 1 => 1
  case n => fib(n-1) + fib(n-2)
}*/
object CachingSolution {
  var mutableMap = Map[List[Int], Boolean]()

  def isWinningPosition(list: List[Int]): Boolean = mutableMap.get(list) match {
    case Some(cachedResult) => cachedResult
    case None => possibleMoves(list) match {
      //no possible moves
      case List() => false
      // exists at least one move that is loosing for the next player
      case moves if !moves.forall(isWinningPosition(_)) => true
      // all moves for other player are winning, we loose whatever we choose now
      case _ => false
    }
  }


  /*  def cachingIsWinningPosition(list: List[Int]): Boolean = mutableMap.get(list) match {
      case Some(result) => result
      case None => {
        val result = cachingIsWinningPosition(list)
        mutableMap = mutableMap + (list -> result)
        result
      }
    }*/
}

assert(!CachingSolution.isWinningPosition(List(1, 0, 0, 1)))
assert(CachingSolution.isWinningPosition(List(1, 0, 1, 0, 1)))
assert(!CachingSolution.isWinningPosition(List(0, 0, 0, 0, 0, 0)))
*/


//OK, but I guess that I also should make 'possibleMoves(list)' more efficient?
//or replace some things into tail recursions?

//OK, looks like I modified the possible moves to using patch and also distinct and it works better
//Now it's time to wrap this beauty into the Solution class that would be able to read inputs from hackerrank

//Well, my solution was terminated due to a timeout... what will speed it up though?
//Well, i think i got to make 'possibleMoves' also cacheable, since it is also called a lot for same inputs.

//Well, inially i forgot to update maps, but now I am still running out of allotted 7s
//What could be improved? Maybe more effective reading?

object Solution {
  var cachedGameStateEvaluations = Map[List[Int], Boolean]()
  var cachedPossibleMoves = Map[List[Int], List[List[Int]]]()

  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines() drop 1

    input.sliding(2,2).foreach(inputPair => {
      val line = inputPair.tail.head

      val list = line.split(" ").map(_.toInt).toList

      if (isWinningGameState(list))
        print("Alice\n")
      else print("Bob\n")
    })

  }

  def isWinningGameState(gameState: List[Int]): Boolean = cachedGameStateEvaluations.get(gameState) match {
    case Some(cachedResult) => {
//      println("cached gamestate evaluation used for " + gameState.toString)
      cachedResult
    }
    case None => {

      val gameStateEvaluation = possibleMoves(gameState) match {
        //no possible moves
        case List() => false
        // exists at least one move that is loosing for the next player
        case moves if moves.exists(!isWinningGameState(_)) => true
        // all moves for other player are winning, we loose whatever we choose now
        case _ => false
      }

      cachedGameStateEvaluations = cachedGameStateEvaluations + (gameState -> gameStateEvaluation)
//      println("update to cached gamestate evaluations; new is: " + gameState.toString)
      gameStateEvaluation
    }
  }

  def possibleMoves(list: List[Int]): List[List[Int]] = cachedPossibleMoves.get(list) match {
    case Some(cachedMoves) => {
//      println("cached possible moves used for: " + list.toString)
      cachedMoves
    }
    case None => {
      def checkIfValid(triple: List[Int]): Boolean = triple.head == 0 && triple.last == 0

      val innerChecked = list.sliding(3).map(checkIfValid(_)).toList
      val indexedChecks = innerChecked.zipWithIndex.filter(_._1)

      val availableMoves = indexedChecks.map((pair: (Boolean, Int)) => list.patch(pair._2, Nil, 1)).distinct

      cachedPossibleMoves = cachedPossibleMoves + (list -> availableMoves)
//      println("updating cached possible moves; new is: " + list.toString)
      availableMoves
    }
  }
}
/*
assert(!Solution.isWinningGameState(List(1, 0, 0, 1)))
assert(Solution.isWinningGameState(List(1, 0, 1, 0, 1)))*/
//assert(!
  Solution.isWinningGameState(List(0,0,1, 0,1,1,1,1,0,1, 0, 1,1, 0, 0, 0))
//)

