import java.io.ByteArrayInputStream

//https://www.hackerrank.com/challenges/functions-or-not
abstract class State
case class NotViolatedState(map: Map[Int, Int]) extends State
case class ViolatedState() extends State
case class GroupDoneState() extends State

def parseLine(line: String) = {
  val res = line.split("\\s+")
    .map(_.toInt)
    .toList
  res
}

def process(state: State, message: String): State = {
  val input = parseLine (message)
//  println("processing input = " + input + " from state = " + state)

  state match {
    case NotViolatedState(map) => {
      input match {
        case x::Nil => {
          println("Yes")
          GroupDoneState()
        }
        case x::y::Nil if (map.contains(x) && map.get(x) != y) => ViolatedState()
        case x::y::Nil => NotViolatedState(map + (x -> y))
      }
    }

    case ViolatedState() => {
      input match {
        case x::Nil => {
          println("No")
          GroupDoneState()
        }
        case _ => ViolatedState()
      }
    }

    case GroupDoneState() => {
      input match {
        case x::Nil => GroupDoneState()
        case x::y::Nil => NotViolatedState(Map(input.head -> input.tail.head))
      }
    }
  }
}


parseLine("1 2 3")

object Solution {

  def processInput(input: List[String]) = {
    (input.drop(1):::List("0")).foldLeft[State](GroupDoneState())(process)
  }

  def main(args: Array[String]) {
    println("main is executing")
    val lines = io.Source.stdin.getLines().toList
    processInput(lines)
  }

}

val testInput:List[String] = List("2", "3", "1 1", "2 2", "3 3", "4", "1 2", "2 4", "3 6", "4 8")
/*
2
3
1 1
2 2
3 3
4
1 2
2 4
3 6
4 8  */


Solution.processInput(testInput)