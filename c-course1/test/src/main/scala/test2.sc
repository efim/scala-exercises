/*
Area under curve
 */

val step = 0.001

// This function will be used while invoking "Summation" to compute
// The area under the curve.
def f(coefficients:List[Int],powers:List[Int],x:Double):Double =
{
  val polynom = coefficients zip powers

  polynom.map((term:(Int, Int)) => term._1 * math.pow(x, term._2)).sum
}

//f(List(1,1,1), List(1,2,3), 2)

// This function will be used while invoking "Summation" to compute
// The Volume of revolution of the curve around the X-Axis
// The 'Area' referred to here is the area of the circle obtained
// By rotating the point on the curve (x,f(x)) around the X-Axis
def area(coefficients:List[Int],powers:List[Int],x:Double):Double =
{
  //lol across x-axis, imagine that and it is easy
  val func_x = f(coefficients, powers, x)
  math.Pi * math.pow(func_x, 2)
}

// This is the part where the series is summed up
// This function is invoked once with func = f to compute the area         // under the curve
// Then it is invoked again with func = area to compute the volume
// of revolution of the curve
def summation(func:(List[Int],List[Int],Double)=>Double,upperLimit:Int,lowerLimit:Int,coefficients:List[Int],powers:List[Int]):Double =
{
  val points = Stream.from(0).map(_ * step + lowerLimit).takeWhile(_ < upperLimit)

  points.map(func(coefficients, powers, _)*step).sum

}

summation(f, 4, 1, List(1), List(0))
summation(f, 2, 1, List(1), List(1))



/*val coefs = List(1, 2, 3, 4, 5)
val powers = List(6, 7, 8, 9, 10)
val lowerLimit = 1
val upperLimit = 4


summation(f, upperLimit, lowerLimit, coefs, powers)
summation(area, upperLimit, lowerLimit, coefs, powers)*/

//fibo

def fibonacci(x:Int):Int = x match {
  case 0 => 0
  case 1 => 1
  case _ => fibonacci(x-1) + fibonacci(x-2)
}


def fibo(x:Int):Int = {
  def fiboAcc(x: Int, prev:Int, prevPrev:Int):Int = x match {
    case 0 => 0
    case 1 => 1
    case 2 => prev
    case _ => fiboAcc(x-1, prev + prevPrev, prev)
  }

  fiboAcc(x, 1, 0)
}

fibo(4)

def pascal(n: Int):List[Int] = {
  def pascal(n: Int, acc: List[Int]):List[Int] = n match {
    case 1 => acc
    case _ => pascal(n-1, update(acc))
  }

  def update(row: List[Int]): List[Int] = {
    val zipped = 0::row zip row:::List(0)
    zipped.map((pair:(Int, Int)) => pair._1 + pair._2)
  }

  pascal(n, List(1))
}

val pascals = Stream.from(1).map(pascal).map(_.mkString(" "))

val n = 5
pascals take n foreach println


//String mingling

val s1 = "abcd"
val s2 = "efgh"

def mingle(s1:String, s2:String):String = {
  val l1 = s1.toList
  val l2 = s2.toList
  val zipped = l1 zip l2

  zipped.flatMap((pair:(Char, Char)) => List(pair._1, pair._2)).mkString
}

mingle(s1, s2)