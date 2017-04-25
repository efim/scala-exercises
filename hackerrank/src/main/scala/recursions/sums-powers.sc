object Solution {
  def from(n: Int):Stream[Int] = n #:: from(n+1)
  //note: other way to write Stream.cons(n, from(n+1))

  def powers(n:Int) = from(1).map(scala.math.pow(_,n))

  def countSumPaths(n: Int, summants: Stream[Double]): Int = {
    //summants are expected to be sorted in increasing order
    def countSumPaths(n: Int, summants: Stream[Double], acc: Double): Int = {
      if (summants.head + acc > n) 0
      else if (summants.head + acc == n) 1
      else {
        val withHead = countSumPaths(n, summants.tail, acc+summants.head)
        val withoutHead = countSumPaths(n, summants.tail, acc)
        withHead + withoutHead
      }
    }

    countSumPaths(n, summants, 0)
  }

  def numberOfWays(X:Int,N:Int):Int = {
    countSumPaths(X, powers(N))
  }

  def main(args: Array[String]) {
    println(numberOfWays(readInt(),readInt()))
  }
}



def from(n: Int):Stream[Int] = n #:: from(n+1)
//note: other way to write Stream.cons(n, from(n+1))

def powers(n:Int) = from(1).map(scala.math.pow(_,n))

//powers(2).take(10).toList
def countSumPaths(n: Int, summants: Stream[Double]): Int = {
  //summants are expected to be sorted in increasing order
  def countSumPaths(n: Int, summants: Stream[Double], acc: Double): Int = {
    if (summants.head + acc > n) 0
    else if (summants.head + acc == n) 1
    else {
      val withHead = countSumPaths(n, summants.tail, acc+summants.head)
      val withoutHead = countSumPaths(n, summants.tail, acc)
      withHead + withoutHead
    }
  }

  countSumPaths(n, summants, 0)
}

def countPowerSumPaths(n: Int, pow: Int):Int = countSumPaths(n, powers(pow))

//countPowerSumPaths(10, 2)
//countPowerSumPaths(100, 2)