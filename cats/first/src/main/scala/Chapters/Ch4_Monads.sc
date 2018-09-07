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