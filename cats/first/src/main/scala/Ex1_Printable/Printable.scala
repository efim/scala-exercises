package Ex1_Printable

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit proof: Printable[A]): String = proof.format(value)
  def print[A: Printable](value: A): Unit = println(implicitly[Printable[A]].format(value))
}

object PrintableInstances {
  implicit object PrintableInt extends Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit object PrintableString extends Printable[String] {
    override def format(value: String): String = value
  }
}