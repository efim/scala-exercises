/*
Solving problems from http://aperiodic.net/phil/scala/s-99/
 */
import java.util.NoSuchElementException

/* P01
Find the last element of a list.
 */
object P01 {
  def last(list: Any): Any = list match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}



/*
P02
Find the last but one element of a list.
 */
object P02 {
  def penultimate(list: Any): Any = list match {
    case head :: Nil => throw new NoSuchElementException
    case _ :: elem :: Nil => elem
    case _ => throw new NoSuchElementException
  }
}
