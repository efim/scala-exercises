/*
package calculator

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._


/**
  * Created by efim on 10.06.17.
  */
abstract class QuickCheckPolynomial extends Properties("Polynomial") {

  import calculator.Polynomial._

  property("computes delta") = forAll { (a: Double, b: Double, c: Double) =>
    val computed = computeDelta(new Signal(a), new Signal(b), new Signal(c))()
    computed - (scala.math.pow(b,2) + 4*a*c) < 0.001
  }

  type Delta = Double
  val deltaGen: Gen[Delta] = Gen.choose(min = 0.001, max = 0.3)

/*  property("computes solutions") = forAll { (a: Double, b: Double, c: Double, d: Delta) =>
    def equation(x: Double) = a*x*x + b*x + c

    val solutions = computeSolutions(new Signal(a),new Signal(b),new Signal(c), new Signal(d))()

    solutions.forall(equation(_) < d)
  }*/

  property("number of solutions by delta")  = forAll { (a: Double, b: Double, c: Double, d: Double) =>

    val solutions = computeSolutions(new Signal(a),new Signal(b),new Signal(c), new Signal(d))()

    val solutionsNumber = solutions.size

    (d < 0 && solutionsNumber == 0) ||
      (d == 0 && solutionsNumber == 1) ||
      (d > 0 && solutionsNumber == 2)

  }
}

*/
