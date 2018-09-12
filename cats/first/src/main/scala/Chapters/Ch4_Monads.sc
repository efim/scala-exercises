/*
Informallly everything with
- unit (constructor from pure value) (A => F[A])
- flatMap F[A] => (A => F[B]) => F[B]

did I remember this correctly?
'mechanism for sequencing computations'

Functors take in 'pure values' so any complications of additional context are only present in the end? beginning?
with monads method flatMap hadles intermediate complication

processing Option with function ff passed to flatMap, function ff only has to specify what happens to 'ordinary' value
sutiation of passing in None is handled by flatMap itself

map - is a sequenced computation that 'does not' introduce new monad,
flatMap - introduces new monad


 */

/** 4.3.1 Exercise: Monadic Secret Identities!
    Implement pure , map , and flatMap for Id
*/

type Id[A] = A

trait myIdMonad[A] {
    def map[B](item: Id[A])
              (f: A => B): Id[B]

    def flatMap[B](item: Id[A])
                  (f: A => Id[B]): Id[B]

    def pure(value: A): Id[A]
}

def myIdMonad[A](): myIdMonad[A] = new myIdMonad[A] {
    def pure(value: A): Id[A] = value

    def map[B](item: Id[A])(f: A => B): Id[B] = f(item: A)

    def flatMap[B](item: Id[A])(f: A => Id[B]): Id[B] = f(item: A)
}

val idInt = 1: Id[Int]

val myIdIntMonad = myIdMonad[Int]()

val idInt2 = myIdIntMonad.pure(1)
myIdIntMonad.map(idInt2)(_.toString)
myIdIntMonad.flatMap(idInt)( int => int.toDouble: Id[Double])

/**
  * Cats have syntax for Either, to backport default-right behaviour to 2.11 and lower
  * also syntax to create val.asRight and val.asLeft
  * to mark resulted value as Either and not Right or Left
  * and giving ability to immediately specifying other type
  * stopped at page 93
  */

/// Either cats syntax useful things:
import java.security.KeyStore.PasswordProtection

import cats.syntax.either._

sealed trait LoginError extends Serializable
final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError
case object UnexpectedError extends LoginError

case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

def handleError(error: LoginError): Unit =
    error match {
        case UserNotFound(u) =>
            println(s"User not found: $u")
        case PasswordIncorrect(u) =>
            println(s"Password incorrect: $u")
        case UnexpectedError =>
            println(s"Unexpected error")
    }

val result1 = User("nn", "123").asRight
val result2 = UserNotFound("nnn").asLeft

result1.fold(handleError, println)
result2.fold(handleError, println)

// also new are
result2.recover {case error: LoginError => User("default", "") }
PasswordIncorrect("lanam").asLeft.leftMap(value => f"lolo: $value")

result1.bimap((left: LoginError) => "mapped! " + left.toString(), (right: User) => right.username)

result2.bimap((left: LoginError) => "mapped! " + left.toString(), (right: User) => right.username)

/// The Eval monad ... abstraction over methods of evaluation
// {eager, lazy} X {memoized, not memoized}.

// cats has Now, Later, Always
import cats.Eval

val now = Eval.now({println("hi NOW!"); math.random + 1000}) // eager, memoized
val later = Eval.later({println("hi LATER!"); math.random + 2000}) // lazy, memoized
val always = Eval.always({println("hi ALWAYS!"); math.random + 3000}) // lazy,

now.value
now.value

later.value
later.value

always.value
always.value

// map and flatMap on Eval...
val greeting = Eval.
    always { println("Step 1"); "Hello"}.
    map { str => println("Step 2"); s"$str world"}

greeting.value

val ans = for {
    a <- Eval.now {println("Calculating A"); 40 }
    b <- Eval.always {println("Calculating B"); 2 }
} yield {
    println("Adding A and B")
    a + b
}

ans.value

ans.value

// stopped at p.106

/// Exercise: safer folding using Eval

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) {
        (a, b) => b.map(fn(a, _))
    }.value

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
        case head :: tail =>
            Eval.defer(fn(head, foldRight(tail, acc)(fn)))
        case Nil =>
            acc
    }

// i'll be damned if I say that this is intuitive for me right now, let's revisit tomorrow.