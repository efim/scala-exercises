package Ex1_Printable

import cats.Show
import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit object PrintableCat extends Printable[Cat] {
    override def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year old ${cat.color} cat."
  }

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year old ${cat.color} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.instance( (leftCat, rightCat) =>
      (leftCat.name === rightCat.name)
        && (leftCat.age === rightCat.age)
        && (leftCat.color === rightCat.color)
    )
}

