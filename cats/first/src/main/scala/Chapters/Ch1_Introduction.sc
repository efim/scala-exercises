// importing type class
import cats.Show
// importing instances of type class for types
import cats.instances.int._
// there are : int, string, list, option, *, all

val showInt: Show[Int] = Show.apply[Int]
val intToString: String = showInt.show(1)

import cats.instances.string._
val showString: Show[String] = Show.apply[String]
val stringToString: String = showString.show("Hello!")

// using syntax:
import cats.syntax.show._

val shownInt = 123.show
val shownString = "Another Hello!".show
// See: there is actually something with name Shown in implementation
// for now I'm not sure what it does

//Shorter shotgun imports:
// import cats._ - imports all type classes
// import cats.instances.all._ - all type class instances
// import cats.syntax.all._ - all of syntax
// import cats.implicits._ - all of instances *and* syntax

// stopped at p.41
import cats.Monoid
import cats.instances.int._

Monoid[Int].combine(1,2)

import cats.instances.option._
Monoid[Option[Int]].combine(Some(4), None)
Monoid[Option[Int]].empty
//that's disconcerning, so there are TWO neutral elements...
//and they are not equal

// using monoid syntax
import cats.syntax.semigroup._
import cats.instances.string._

val stringResult = "Hi " |+| "there" |+| Monoid[String].empty

// so difference is that this operation is guaranteed to be inside some type?
// there got to be some reusable code that is written against semigroup / monoid then

import cats.instances.int._
val intResult = 1 |+| 2 |+| Monoid[Int].empty

def add[A](items: List[A])(implicit p: Monoid[A]): A = {
  items.foldRight(p.empty)(_ |+| _)
}

add(List(1,2,3,4,5))
add(List(Some(1), Some(2), None, None, Some(3)))
//add(List(Some(1), Some(2), Some(3))) - error - List[Some[Int]] - no implicit found


// now making our generic code working with the new case class:
import cats.instances.double._
case class Order(totalCost: Double, quantity: Double)
implicit object OrderMonoid extends Monoid[Order] {
  override def empty = Order(0, 0)
  override def combine(x: Order, y: Order) =
    Order(
      x.totalCost |+| y.totalCost,
      x.quantity |+| x.quantity
    )
}

add(List(Order(1,2), Order(1,1), Order(50, 2)))

/**
  so whatever there is awailable for monoids (and semigroups) I can reuse if I implement
  the typeclass, let this sink deep

  Monoid = abstraction of the concept of adding and combining

  * Hadoop Spark - data calculated in partitions that need to be combined
  total visitors - (Int, +)
  unique visitors - (Set[User], union)
  99% and 95% response times from logs = (QTree, ???)
  therefore framework that allows running monoid based analysis is powerful and generic
  see Twitter Algebird and Summingbird projects, map-reduce case study

  * CDRT - Commutative replicated data types
  data types that are distributed and can have uneven updates
  and provide **eventual consistency** by combining all available eventable updates on merge

*/

import cats.Monoid
import cats.instances.string._
import cats.syntax.semigroup._

"Scala " |+| "with " |+| "Cats"

import cats.instances.int._
import cats.instances.option._

Option(1) |+| Option(2)

import cats.instances.map._

val map1 = Map("a" -> 1, "b" -> 2)
val map2 = Map("b" -> 3, "d" -> 4)

map1 |+| map2
// semigroup by adding values of keys - commutative

import cats.instances.tuple._

val tuple1 = ("hello", 123)
val tuple2 = ("world", 321)

tuple1 |+| tuple2

//On this fun note timer calls me back to work, and someday I'll return already to Chapter 3