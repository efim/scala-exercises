object Solution {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var q = sc.nextInt();
    var a0 = 0;
    while (a0 < q) {
      var n = sc.nextInt();
      var a = new Array[Int](n);
      for (a_i <- 0 to n - 1) {
        a(a_i) = sc.nextInt();
      }
      if (isPrettySortable(a.toList))
        print("Yes\n")
      else print("No\n")
      a0 += 1;
    }
  }

  def isPrettySortable(s: List[Int]): Boolean = s match {
    case x :: y :: z :: tail if x > y && y > z => false
    case x :: y :: tail if x - y > 1 => false
    case x :: Nil => true
    case x :: tail => isPrettySortable(tail)
  }
}


assert(Solution.isPrettySortable(List(1, 0, 3, 2)))
assert(!Solution.isPrettySortable(List(2, 1, 0)))
//assert(!!Solution.isPrettySortable(List(1, 2, 0, 4, 3)))
