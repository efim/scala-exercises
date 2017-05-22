import Solution.{superDigit, superDigitIter}

import scala.annotation.tailrec

//superDigit(148148148)

object Solution {

  @tailrec
  def superDigit(number: BigInt): BigInt = {
//    println(number)
    val digits = number.toString.toList.map(_.toInt - 48)
//    println(number.toString.toList)
//    println(digits)
    digits match {
      case digit::Nil => digit
      case _ => superDigit(digits.sum)
    }
  }

  def superDigitIter(number: BigInt): BigInt = {
    val digits = number.toString.toList.map(_.toInt - 48)
    digits.sum
  }

  def main(args: Array[String]) {
    val Array(n, k) = io.Source.stdin.getLines().next().split(" ")

    println(superDigit(superDigitIter(BigInt(n)) * k.toInt))
  }
}

print(superDigitIter(BigInt("861568688536788")))
println(superDigit(superDigitIter(BigInt("861568688536788")) * 100))




