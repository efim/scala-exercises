package Ex1_Printable

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit proof: Printable[A]): String = proof.format(value)
  def print[A](value: A)(implicit proof: Printable[A]): Unit = println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit proof: Printable[A]): String = proof.format(value)
    def print(implicit proof: Printable[A]): Unit = println(proof.format(value))
  }
}

object PrintableInstances {
  implicit object PrintableInt extends Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit object PrintableString extends Printable[String] {
    override def format(value: String): String = value
  }
}