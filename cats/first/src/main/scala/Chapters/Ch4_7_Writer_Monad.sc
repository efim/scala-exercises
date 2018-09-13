import cats.data.Writer
import cats.instances.vector._

Writer(Vector(
  "String1",
  "String222"
), 99911)

import cats.syntax.applicative._

type Logged[A] = Writer[Vector[String], A]

123.pure[Logged] // is have result and no log

import cats.syntax.writer._ // for tell

Vector("msg1", "msg2", "msg3").tell

// if have both then two ways to initialize

val a = Writer(Vector("msg1", "msg2", "msg3"), 123)

val b = 123.writer(Vector("msg1", "msg2", "msg3"))

val aResult: Int = a.value
val aLog: Vector[String] = a.written
val (log, result) = b.run

// log is preserved through map and flatMap operations over Writer
// flatMap appends the logs of source Writer and result of sequenced function

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

// additionally log can be transformed by mapWritten:
val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
writer2.run

/** also
  * bimap - takes two functions and maps both
  * mapBoth - takes one function from (log, result)
  * reset - resets log to empty
  * swap - swaps them
   */


/// Exercise
def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Logged[Int] = {
  for {
    ans <- if(n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n)
    _ <- Vector(s"fact $n $ans").tell
  } yield ans
}

factorial(3).run

