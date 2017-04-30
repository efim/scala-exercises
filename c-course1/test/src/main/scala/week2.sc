//# Streams
val myStream = Stream.cons(1, Stream.cons(2, Stream.empty))

myStream.foreach(println)

//manual construction of streams via recursive function
//by design Stream will evaluate tail only on demand, by calling function by name
def rangeToStream(low:Int, high:Int):Stream[Int] = {
  if (low > high) Stream.empty
  else Stream.cons(low, rangeToStream(low+1, high))
}

rangeToStream(1,4).foreach(println)

print(rangeToStream(2,51))

val otherStream = 1 #:: 2 #:: 4 #:: 5 #:: Stream.empty


/*# Lazy evaluation
Differs from 'by name' in that it caches computed value for further reference
not a default in Scala, use as

 */

lazy val lazyRange = {
  println("lazy range")
  1 to 5
}

def byNameRange = {
  println("by name range")
  1 to 5
}

val test1 = lazyRange
val test2 = lazyRange
val test3 = byNameRange
val test4 = byNameRange

//So here is not completely what I expected.
//lazy val was computed at the place of definition


def expr = {
  val x = {println('x'); 1}
  lazy val y = {println('y'); 2}
  def z = {println('z');3}

  z + x + y + z + x + y + z
}

expr

lazy val N:Stream[Int] = Stream.cons(1, N.map(_+1))

print(N.take(10).toList)
















