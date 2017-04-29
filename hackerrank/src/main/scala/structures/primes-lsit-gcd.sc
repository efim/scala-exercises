class Number(val decomposition: Map[Int, Int]) {
  def this(rawDecomposition: List[Int]) = this(processRaw(rawDecomposition))

  def processRaw(rawDecomposition: List[Int]) = {
    rawDecomposition.sliding(2, 2).map(_ match {
      case List(a,b) => a -> b
    }).toMap
  }
}

def gcd(a: Number, b: Number):Number = {
  val sharedPrimes = a.decomposition.keySet.intersect(b.decomposition.keySet)

  val gcdDecomposition = sharedPrimes.map(key => {
    val aPower = a.decomposition.getOrElse(key, 0)
    val bPower = b.decomposition.getOrElse(key, 0)

    (key -> (if (aPower < bPower) aPower else bPower))
  }).toMap

  new Number(gcdDecomposition)
}

