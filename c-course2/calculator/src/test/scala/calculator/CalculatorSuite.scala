package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import org.scalatest.prop.Checkers
import org.scalacheck.Test._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers with Checkers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("number of solutions by delta") {

    import calculator.Polynomial._

    val (a,b,c) = (1.0,2.2,3.3)
    val d = computeDelta(new Signal(a), new Signal(b), new Signal(c))

    val solutions = computeSolutions(new Signal(a),new Signal(b),new Signal(c), d)()

    val solutionsNumber = solutions.size

    assert((d() < 0 && solutionsNumber == 0) ||
      (d() == 0 && solutionsNumber == 1) ||
      (d() > 0 && solutionsNumber == 2))
  }

  test("computes delta") {
    import calculator.Polynomial._

    val (a,b,c) = (1.3, 5.3, 6)
    val computed = computeDelta(new Signal(a), new Signal(b), new Signal(c))()
    computed - (scala.math.pow(b,2) - 4*a*c) < 0.001
  }

  test("NaN on circular") {
    import calculator.Calculator._

    val results = computeValues(Map(
      "a" -> Signal { Ref("a") },
      "b" -> Signal { Literal(1)},
      "c" -> Signal { Plus(Ref("b"), Ref("a")) }
    ))

    assert(results("a")().equals(Double.NaN))
    assert(results("c")().equals(Double.NaN))

  }

/*  test("running properties for some reason here") {
    checkProperties(Parameters.default, new QuickCheckPolynomial {})
  }*/

}
