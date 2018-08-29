// exercise: making monoids from booleans, sets by hand

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}


// laws
def associativeLaw[A](x: A, y: A, z: A)
                     (implicit m: Monoid[A]): Boolean = {
  m.combine(x, m.combine(y, z)) ==
    m.combine(m.combine(x, y), z)
}
def identityLaw[A](x: A)
                  (implicit m: Monoid[A]): Boolean = {
  (m.combine(x, m.empty) == x) &&
    (m.combine(m.empty, x) == x)
}

implicit object BooleanAndMonoid extends Monoid[Boolean] {
  override def empty: Boolean = true

  override def combine(x: Boolean, y: Boolean) = x && y
}

implicit object BooleanOrMonoid extends Monoid[Boolean] {
  override def empty: Boolean = false

  override def combine(x: Boolean, y: Boolean) = x || y
}

// forgot about (exclusiveOr, false) and (exclusiveXor, true)


implicit def setUnionMonoid[A]: Monoid[Set[A]] =
  new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  }

val intSetMonoid = Monoid[Set[Int]]



intSetMonoid.combine(Set(1,2), Set(2,3))

// missed (union - intersection, empty)
// -----------------------------------

import cats.Semigroup
import cats.Monoid

//import cats.implicits._
//Monoid[String].combine("hi", "there")
// there is not implicit for Monoid[String] in cats.instances.string._

import cats.instances.int._
Monoid[Int].combine()
