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
    ans <- if(n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  } yield ans
}

factorial(3).run


/**
  * Reader monad. Sequencing operations that depend on some input
  * wrap functions of one argument, giving us useful methods for composing them
  *
  * Common use is dependency injection:
  * if many functions take external config, Reader can help us combine them
  * in large operation that takes config
  */

import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] = Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

// combining readers....
// map passing result of first reader as input into second, still what is
// benefit comparing to function (as functor)?

val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello, $name")

greetKitty.run(Cat("Heathcliff", "junk food"))

// flatMap allows to combine readers that depend on the same input type

val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "lasagne"))

// classic use or Reader is to build programs that accept a configuration as
// a parameter

case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
             )

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(
    db => db.usernames.get(userId)
  )

def checkPassword(
                 username: String,
                 password: String): DbReader[Boolean] =
  Reader(
    db => db.passwords.get(username).contains(password)
  )

def checkLogin(
                userId: Int,
                password: String): DbReader[Boolean] =
  for {
    username <- findUsername(userId)
    check <- username
      .map(name => checkPassword(name, password))
      .getOrElse(false.pure[DbReader])
  } yield check

/**
  * def checkLogin(
  *     userId: Int,
  *     password: String): DbReader[Boolean] =
  *   for {
  *     username <- findUsername(userId)
  *     passwordOk <- username.map { username =>
  *       checkPassword(username, password)
  *     }.getOrElse {
  *       false.pure[DbReader]
  *     }
  *   } yield passwordOk
  */

// almost as solution from the book,
// forgetting to use pretty Pure stuff, especially with .pure[DbReader] syntax

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo")

val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret")

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)

checkLogin(4, "davinci").run(db)
// stopped at page 118















