package Ex1_Printable

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit object PrintableCat extends Printable[Cat] {
    override def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year old ${cat.color} cat."
  }

}

