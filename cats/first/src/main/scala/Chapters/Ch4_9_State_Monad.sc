/**
  * In simplest terms instances of State[S, A]
  * represent functions of type S => (S, A)
  * S is the type of state and A is the type of result
  */

import cats.data.State

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

val (state, result) = a.run(10).value
val onlyState = a.runS(10).value
val onlyResult = a.runA(10).value

// map and flatMap thread the state from one instance to another
// each instance represents atomic state transformation

val step1 = State[Int, String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
  c <- step1
} yield (a, b, c)

val (bothState, bothResult) = both.run(20).value

/**
  * General usage of State monad is to represent each step of a
  * computation as an instance and compose the steps
  *
  * Convenience constructors for primitive steps:
  * - get - extracts state as result
  * - set - updates state and returns unit as result
  * - pure - ignores the state and returns supplied result
  * - inspect - extracts the state as result via a transformation function
  * - modify - updates the state using an update function
  */

val getDemo = State.get[Int]
getDemo.run(10).value

val setDemo = State.set[Int](30)
setDemo.run(10).value

val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

val inspectDemo = State.inspect[Int, String](_ + "!")
inspectDemo.run(10).value

val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

import State._

val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)

val (programState, programResult) = program.run(1).value

// now some kind of exercise for building FSM ... caclulator for postfix notation

type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
  val symbol = sym.charAt(0).charValue() // charAt is not good at all
  val operations = Map(
    '+' -> { a: Int => b: Int => a + b },
    '*' -> { a: Int => b: Int => a * b }
  )

  if (symbol.isDigit) {
    val digit = symbol.toInt
    (digit :: oldStack, digit)
  } else if (operations.contains(symbol)) {
    oldStack match {
      case x :: y :: restStack => (restStack, operations(symbol)(x)(y))
      case _ => throw new IllegalStateException("Not enough operands on stack")
    }
  } else {
    throw new IllegalArgumentException("Unrecognized symbol")
  }
}

def evalDigit(sym: String): CalcState[Int] = {
  val d = sym.toInt
  for {
    _ <- modify[List[Int]](d :: _)
  } yield d
}
/*

def evalOperation(sym: String): CalcState[Int] = {
  val operations = Map(
    '+' -> { a: Int => b: Int => a + b },
    '*' -> { a: Int => b: Int => a * b }
  )

  for {
    oldStack <- get[List[Int]]
    a = oldStack.head
    b = oldStack.tail.head
    _ <- modify[List[Int]](_.tail.tail)
  } yield operations(sym.charAt(0))(a)(b)
}

def evalOneFromPrimitives(sym: String): CalcState[Int] =
  for {
    result <- evalDigit(sym) if sym.charAt(0).isDigit
    result <- evalOperation(sym) if !sym.charAt(0).isDigit
  } yield result
*/


// both solutions are not quite there. here's with idea from the book
// but using primitives

def operator(function: (Int, Int) => Int): CalcState[Int] =
  for {
    oldStack <- get[List[Int]]
    (a, b, restStack) = oldStack match {
      case a :: b :: restStack => (a, b, restStack)
      case _ => throw new IllegalStateException("Not enough arguments on stack")
    }
    result = function(a,b)
    _ <- set[List[Int]](result::restStack)
  } yield result

def anotherOperator(function: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] { state =>
    state match {
      case a :: b :: tail => {
        val result = function(a, b)
        (result::tail, function(a, b))
      }
      case _ => throw new IllegalStateException("Not enough arguments on stack")
    }
  }

def operand(i: Int): CalcState[Int] =
  for {
    _ <- modify[List[Int]](i :: _)
  } yield i

def evalOneBookVersion(sym: String): CalcState[Int] =
  sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

evalOne("42").run(Nil).value
//evalOneFromPrimitives("42").runA(Nil).value
println("test evalOne book version updated")
evalOneBookVersion("42").run(Nil).value
evalOneBookVersion("+").run(List(1,2)).value

"4".charAt(0).charValue().toInt
// hoho

val calcProgram = evalOneBookVersion("1").flatMap { case _ => evalOneBookVersion("2").flatMap { case _ => evalOneBookVersion("+").map(ans => ans)
}
}


calcProgram.runA(Nil).value

import cats.syntax.applicative._
import cats.instances.int._

def evalAll(input: List[String]): CalcState[Int] = {
  input.foldRight(0.pure[CalcState])(
    (str, state) => state.flatMap(
      { case _ => evalOneBookVersion(str) }
    ))
}


val longProgram = evalAll(List("1", "2", "+", "3", "*"))

//longProgram.runA(Nil).value

def evalAllBookVersion(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (state, str) =>
    state.flatMap(_ => evalOneBookVersion(str))
  }

val longProgram2 = evalAllBookVersion(List("1", "2", "+", "3", "*"))

longProgram2.runA(Nil).value








