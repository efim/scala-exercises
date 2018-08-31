
/**
  Special cases of Functors: Applicative Functor and Monad are commonly used things

  informally - functor is anything with map: A => B
  Function is applied to content, but leaves _context_ unchanged
  */

// for list
List(1,2,3).map(_ + 1)
//context is still a list

// for options
Some(1).map(_ + 1)
val a: Option[Int] = None
a.map(_ + 1)
// contexts are still Some() and None

// Think about map as way to sequence computations
// and ignorign some complications of specific data type
List(1,2,3)
  .map(_ + 1)
  .map(_ * 2)
  .map(_ + "!")


// This sequencing doesn't have to be eager and used in same way with Future and IO
import cats.Functor

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val future: Future[String] =
  Future(123).
    map(_ + 1).
    map(_ * 2).
    map(_ + "!")

Await.result(future, 1 second)

// single value function is also a functor
val func =
  ((x: Int) => x.toDouble)
    .andThen(x => x + 1)
    .andThen(x => x * 2)
    .andThen(x => x + "!")
func(123)
// no more method map on functions?
/** nope
Partial Unification
For the above examples to work we need to add the following compiler
op on to build.sbt :
scalacOptions += "-Ypartial-unification"
otherwise we’ll get a compiler error:
 */

/**
  Functor laws:
  same semantics whether we sequence many small operation one by one or combine them into
  a larger function before mapping

  Identity and Composition
 */

// has method lift - transforms function that operates over values
// into function that operates over functors

import scala.language.higherKinds
import cats.Functor
import cats.syntax.functor._

def doMath[F[_]](start: F[Int])
                (implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

