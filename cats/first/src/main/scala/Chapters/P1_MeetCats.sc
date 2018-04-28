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

