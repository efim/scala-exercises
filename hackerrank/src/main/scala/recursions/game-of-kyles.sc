object Solution {
  val isDebugNew = false
  val isDebugCached = false
  var cachedGameStateEvaluations = Map[List[Boolean], Boolean]()
  var cachedPossibleMoves = Map[List[Boolean], Set[List[Boolean]]]()

  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines() drop 1

    input.sliding(2,2).foreach(inputPair => {
      val line = inputPair.tail.head

      //is the ped standing.
      val list = stringToState(line)

      if (isWinningGameState(list))
        print("WIN\n")
      else print("LOSE\n")
    })
  }

  def stringToState(line: String):List[Boolean] = line.toList.map((c:Char) => if (c == 'X') false else true)

  def hasTerminalWinning(moves: Set[List[Boolean]]): Boolean = {
    // terminal should be actually XXXX, or rather 'contains XXXX', right?
    def isTerminalMove(move: List[Boolean]) = !move.exists(item => item)//!move.foldLeft(false)(_ || _) // after fold we have true if at least one true
    moves.exists(isTerminalMove)
  }

  def isWinningGameState(gameState: List[Boolean]): Boolean = cachedGameStateEvaluations.get(gameState) match {
    case Some(cachedResult) => {
      if (isDebugCached) println("cached gamestate evaluation used for " + gameState.toString)
      cachedResult
    }
    case None => {
      //calculate value
      val gameStateEvaluation = possibleMoves(gameState) match {
        //no possible moves
        case moves if hasTerminalWinning(moves) => true
        // exists at least one move that is loosing for the next player
        case moves if moves.exists(!isWinningGameState(_)) => true
        // all moves for other player are winning, we loose whatever we choose now
        case _ => false
      }

      //don't forget to add to cache
      cachedGameStateEvaluations = cachedGameStateEvaluations + (gameState -> gameStateEvaluation)
      if (isDebugNew) println("update to cached gamestate evaluations; new is: " + gameState.toString)
      if (isDebugNew) println("state evaluation for " + gameState + "is : " + gameStateEvaluation)
      gameStateEvaluation
    }
  }

  def possibleMoves(gameState: List[Boolean]): Set[List[Boolean]] = cachedPossibleMoves.get(gameState) match {
    case Some(cachedMoves) => {
      if (isDebugCached) println("cached possible moves used for: " + gameState.mkString)
      cachedMoves
    }
    case None => {
      //calculate

      val singleMovesList = for {
        place <- gameState.zipWithIndex if place._1
      } yield gameState.patch(place._2, Seq(false), 1)

      val doubleMovesList = for (
        place <- gameState.sliding(2).zipWithIndex if (place._1.head & place._1.tail.head)
      ) yield gameState.patch(place._2, Seq(false, false), 2)

      val availableMoves: Set[List[Boolean]] = singleMovesList.toSet union doubleMovesList.toSet
      //don't forget to cache
      cachedPossibleMoves = cachedPossibleMoves + (gameState -> availableMoves)
      if (isDebugNew) println("updating cached possible moves; new is: " + availableMoves.mkString)
      availableMoves
    }
  }
}

//Solution.isWinningGameState(Solution.stringToState("IXXI"))
//Solution.isWinningGameState(Solution.stringToState("XIIX"))
//Solution.isWinningGameState(Solution.stringToState("IIXII"))
Solution.isWinningGameState(Solution.stringToState("IIIII"))