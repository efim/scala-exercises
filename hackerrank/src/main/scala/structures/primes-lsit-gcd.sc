class Number(decomposition: List[Int]) {

  lazy val primeDecomposition:Map[Int, Int] = {
    decomposition.sliding(2, 2).map(_ match {
      case List(a,b) => a -> b
    }).toMap
  }
}

def gcd(a: Number, b: Number):Number = {
  ???
}