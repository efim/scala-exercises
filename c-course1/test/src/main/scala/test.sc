// filter odd positions
def f1(arr:List[Int]):List[Int] = {
  def filterOddPositionsAcc(list:List[Int], acc:List[Int]):List[Int] = list match {
    case Nil => acc
    case some::Nil => acc
    case first::second::tail => filterOddPositionsAcc(tail, acc ::: List(second))
  }

  filterOddPositionsAcc(arr, List())
}

//filterEven positions
def filterEven(arr:List[Int]):List[Int] =
  arr.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

filterEven(List(1,2,3,4,5,6,7,8))
filterEven(List(2,4,2,4,2,4))
f1(List(1,2,3,4,5,6,7,8))
f1(List(2,4,2,4,2,4))

//make any array of requested size
def f(num:Int) : Array[Int] = {
  val list = (1 to num).toArray
  return list
}

f(4)

//reverse list
def reverse(arr: List[Any]):List[Any] = {
  def reverseAcc(l: List[Any], acc: List[Any]):List[Any] = l match {
    case Nil => acc
    case head::tail => reverseAcc(tail, acc) ::: List(head)
  }

  reverseAcc(arr, List())
}

reverse(List(1,2,3,4,5))

//get sum of odd elements
def f5(arr:List[Int]):Int = arr.filter(_ % 2 != 0).sum

f5(List(1,2,3,4,5,6,-1))

//get len of list
def len(list: List[Any]): Int = list.foldRight(0)((_,acc)=>acc+1)


len(List(1,2,3,4,5))


//update list with absolute values
def f6(arr:List[Int]):List[Int] = arr.map(math.abs(_))

f6(List(1,2,3,-1,-2,-3))


//Evaluating e^x (first 10 terms
def ex(x: Double): Double = {
  val termNumber = 10

  def fact(n: Int):Int = n match {
    case 0 => 1
    case _ => n * fact(n - 1)
  }
  def term(x: Double, n: Int): Double = math.pow(x, n) / fact(n)

  Stream.from(0).take(termNumber).map(num => term(x, num)).sum
}

ex(0)
ex(1)
math.pow(0,0)


//e to power of x
object Solution {

  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines()
    val numOfCases = input.take(1).next.toInt
    val xValues = input.take(numOfCases)
    val eXValues = xValues.map(_.toDouble).map(ex).foreach(println)
    //println(eXValues)
  }

  def ex(x: Double): Double = {
    val termNumber = 10

    def fact(n: Int):Int = n match {
      case 0 => 1
      case _ => n * fact(n - 1)
    }
    def term(x: Double, n: Int): Double = math.pow(x, n) / fact(n)

    Stream.from(0).take(termNumber).map(num => term(x, num)).sum
  }

}