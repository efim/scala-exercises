/*
Solving problems from http://aperiodic.net/phil/scala/s-99/
 */
import java.util.NoSuchElementException
import org.scalatest._

/* P01
Find the last element of a list.
 */
def last(list: Any): Any = list match {
  case head::Nil => head
  case _::tail => last(tail)
  case _ => throw new NoSuchElementException
}

test ("simple list last") {
  last(List(1,2,3,4,5)) === 5
}

/*
P02
Find the last but one element of a list.
 */
def penultimate(list: Any): Any = list match {
  case head::Nil => throw new NoSuchElementException
  case _::elem::Nil => elem
  case _ => throw new NoSuchElementException
}

penultimate(List(1,2,3,4))

penultimate(List(1))

penultimate("some string")

