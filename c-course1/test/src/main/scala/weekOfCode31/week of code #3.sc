object SolutionOverkill {
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

assert(!SolutionOverkill.isWinningGameState(List(1, 0, 0, 1)))
assert(SolutionOverkill.isWinningGameState(List(1, 0, 1, 0, 1)))


//New solution is based on 'why didn't i thought of that myself'
// Step 1 - count long 1s, since they are bound to go away
// All 11s are not going anywhere
// Step 2 - count all 0s that are able to go away.

def countAwailableMoves(gameState: List[Int]): Int = {
  def isALoneInt(triple: List[Int], i:Int): Boolean = triple match {
    case 0::x::0::Nil if x == i => true
    case _ => false
  }

  val (loneOnes, trimmedGameStateTriplets) = gameState.sliding(3).partition(isALoneInt(_,1))

  val loneOnesCount = loneOnes.length

  val availableZeroesCount = trimmedGameStateTriplets.count(isALoneInt(_, 0))

  loneOnesCount + availableZeroesCount
}

def isWinningPosition(gameState: List[Int]): Boolean = {
  countAwailableMoves(gameState) % 2 == 1
}

assert(!isWinningPosition(List(1, 0, 0, 1)))
 assert(isWinningPosition(List(1, 0, 1, 0, 1)))
assert(!isWinningPosition(List(0, 0, 0, 0, 0, 0)))




object Solution {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var q = sc.nextInt();
    var a0 = 0;
    while(a0 < q){
      var n = sc.nextInt();
      var a = new Array[Int](n);
      for(a_i <- 0 to n-1) {
        a(a_i) = sc.nextInt();
      }
      if (isWinningGameState(a.toList))
        print("Alice\n")
      else print("Bob\n")
    }
  }

  def isWinningGameState(gameState: List[Int]): Boolean = {
    countAwailableMoves(gameState) % 2 == 1
  }

  def countAwailableMoves(gameState: List[Int]): Int = {
    def isALoneInt(triple: List[Int], i:Int): Boolean = triple match {
      case 0::x::0::Nil if x == i => true
      case _ => false
    }

    val (loneOnes, trimmedGameStateTriplets) = gameState.sliding(3).partition(isALoneInt(_,1))

    val listOnes = loneOnes.toList
    val listRest = trimmedGameStateTriplets.toList

    val loneOnesCount = listOnes.length

    val middle = listRest.map(_.tail.head)

    assert (loneOnesCount + middle.length + 2 == gameState.length)

    val trimmedGameState = gameState.head::middle ++ List(gameState.last)

    val availableZeroesCount = trimmedGameState.sliding(3).count(isALoneInt(_, 0))

    loneOnesCount + availableZeroesCount
  }
}

Solution.isWinningGameState(List(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0))
Solution.isWinningGameState(List(0,0,1,0,1))
Solution.isWinningGameState(List(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0))
Solution.isWinningGameState(List(0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0))
Solution.isWinningGameState(List(0,1,1,0,0,0,0,1,1,0,0,0))

val gameState = List(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)

def isALoneInt(triple: List[Int], i:Int): Boolean = triple match {
  case 0::x::0::Nil if x == i => true
  case _ => false
}

val (loneOnes, trimmedGameStateTriplets) = gameState.sliding(3).partition(isALoneInt(_,1))

val listOnes = loneOnes.toList
val listRest = trimmedGameStateTriplets.toList

gameState.length

val middle = listRest.map(_.tail.head)
println(middle)

assert (listOnes.length + middle.length + 2 == gameState.length)

val trimmedGameState = gameState.head::middle ++ List(gameState.last)
println(trimmedGameState)

val availableZeroesCount = trimmedGameState.sliding(3).count(isALoneInt(_, 0))