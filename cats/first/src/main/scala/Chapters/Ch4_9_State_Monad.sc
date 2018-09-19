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

val inspectDemo = State.inspect[Int, String] (_ + "!")
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
  def someTransformation(stack: List[Int]): List[Int] = ???
  def someCalculation: Int = ???

  val newStack = someTransformation(oldStack)
  val result = someCalculation

  (newStack, result)
}








