
object Solution {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var w = sc.next();
    // Print 'Yes' if the word is beautiful or 'No' if it is not.

    if( isPretty(w) ){
      print("Yes")
    } else {
      print("No")
    }

  }

  def isPretty(word: String):Boolean = {
    val uglyLetters = Set('a', 'e', 'i', 'o', 'u', 'y')

    val letterPairs = (word + '*') zip ('*' + word)

    val noDoubles = letterPairs.forall((pair:(Char, Char)) => pair._1 != pair._2)
    val noSequentialUglies = letterPairs.forall((pair:(Char, Char)) => (!(uglyLetters.contains(pair._1) && uglyLetters.contains(pair._2))))

    noDoubles && noSequentialUglies
  }
}


assert(Solution.isPretty("abacaba"))
assert(!Solution.isPretty("badd"))
assert(!Solution.isPretty("yes"))

