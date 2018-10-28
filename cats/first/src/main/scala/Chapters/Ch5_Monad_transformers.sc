// combining monads - can't write completely generic flatMap
// to combine two monads some info on one of them is needed
import cats.data.OptionT

type ListOption[A] = OptionT[List, A]
// passing in "outer monad" as a type parameter of monad transformer

import cats.Monad
import cats.instances.list._ // for Monad
import cats.syntax.applicative._ // for pure

val result1: ListOption[Int] = OptionT(List(Option(10), Option(1)))
val result2: ListOption[Int] = 32.pure[ListOption]

// now awailable map and flatMap methods are combining existing ones of List and Option
// otherwise would need to do nested map and flatMap for Some/None
result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

// Each monad transformer is a data type, defined in cats.data

/**
  * Things ot cover:
  * 1. Available transformers
  * 2. How to build stacks of monad types using them
  * 3. How to construct values
  * 4. How to pull apart values to access the wrapped monads
  */
