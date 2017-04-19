object SolutionOverkill {
  var cachedGameStateEvaluations2 = Map[List[Int], Boolean]()
  var cachedPossibleMoves2 = Map[List[Int], List[List[Int]]]()
  var cachedGameStateEvaluations = Map[List[Boolean], Boolean]()
  var cachedPossibleMoves = Map[List[Boolean], List[List[Boolean]]]()

  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines() drop 1

    input.sliding(2,2).foreach(inputPair => {
      val line = inputPair.tail.head

      //is the ped standing.
      val list = line.toList.map((c:Char) => if (c == 'X') false else true)

      if (isWinningGameState(list))
        print("WIN\n")
      else print("LOOSE\n")
    })
  }

  def isWinningGameState(gameState: List[Boolean]): Boolean = cachedGameStateEvaluations.get(gameState) match {
    case Some(cachedResult) => {
      //      println("cached gamestate evaluation used for " + gameState.toString)
      cachedResult
    }
    case None => {
      //calculate value
      val gameStateEvaluation: Boolean = ???


      //don't forget to add to cache
      cachedGameStateEvaluations = cachedGameStateEvaluations + (gameState -> gameStateEvaluation)
      //      println("update to cached gamestate evaluations; new is: " + gameState.toString)
      gameStateEvaluation
    }
  }

  def possibleMoves(gameState: List[Boolean]): List[List[Boolean]] = cachedPossibleMoves.get(gameState) match {
    case Some(cachedMoves) => {
      //      println("cached possible moves used for: " + list.toString)
      cachedMoves
    }
    case None => {
      //calculate
      val availableMoves:List[List[Boolean]] = ???


      //don't forget to cache
      cachedPossibleMoves = cachedPossibleMoves + (gameState -> availableMoves)
      //      println("updating cached possible moves; new is: " + list.toString)
      availableMoves
    }
  }


  def isWinningGameState2(gameState: List[Int]): Boolean = cachedGameStateEvaluations2.get(gameState) match {
    case Some(cachedResult) => {
      //      println("cached gamestate evaluation used for " + gameState.toString)
      cachedResult
    }
    case None => {

      val gameStateEvaluation = possibleMoves2(gameState) match {
        //no possible moves
        case List() => false
        // exists at least one move that is loosing for the next player
        case moves if moves.exists(!isWinningGameState2(_)) => true
        // all moves for other player are winning, we loose whatever we choose now
        case _ => false
      }

      cachedGameStateEvaluations2 = cachedGameStateEvaluations2 + (gameState -> gameStateEvaluation)
      //      println("update to cached gamestate evaluations; new is: " + gameState.toString)
      gameStateEvaluation
    }
  }

  def possibleMoves2(list: List[Int]): List[List[Int]] = cachedPossibleMoves2.get(list) match {
    case Some(cachedMoves) => {
      //      println("cached possible moves used for: " + list.toString)
      cachedMoves
    }
    case None => {
      def checkIfValid(triple: List[Int]): Boolean = triple.head == 0 && triple.last == 0

      val innerChecked = list.sliding(3).map(checkIfValid(_)).toList
      val indexedChecks = innerChecked.zipWithIndex.filter(_._1)

      val availableMoves = indexedChecks.map((pair: (Boolean, Int)) => list.patch(pair._2, Nil, 1)).distinct

      cachedPossibleMoves2 = cachedPossibleMoves2 + (list -> availableMoves)
      //      println("updating cached possible moves; new is: " + list.toString)
      availableMoves
    }
  }
}

assert(!SolutionOverkill.isWinningGameState2(List(1, 0, 0, 1)))
assert(SolutionOverkill.isWinningGameState2(List(1, 0, 1, 0, 1)))