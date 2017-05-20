package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(QuickCheckHeap.this.empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("correct non empty") = forAll { (heap: H) =>
    !isEmpty(insert(0, heap))
  }

  property("empty is empty") = {
    isEmpty(QuickCheckHeap.this.empty)
  }

  property("correct findMin") = forAll { (heap: H) =>
    if (isEmpty(heap)) {
      Prop.throws(classOf[NoSuchElementException]) {
        findMin(heap)
      }
    } else {
      findMin(heap)
      true
    }
  }

  property("inserting several into empty") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, QuickCheckHeap.this.empty))) == scala.math.min(a, b)
  }

  property("adding and removing from empty") = {
    isEmpty(deleteMin(insert(0, QuickCheckHeap.this.empty)))
  }

  //This one is the master properly, lol
  //add property that for any list of ints inserts them and expects to get back in same odrer
  property("extracting inserted list as ordered") = forAll { (list: List[Int]) => {
      val heap = list.foldRight(QuickCheckHeap.this.empty)(insert)

      checkWithSorted(list.sorted, heap)
    }
  }

  def checkWithSorted(list: List[Int], heap: H):Boolean = list match {
    case List() => true
    case x::tail => {
      x == findMin(heap) && checkWithSorted(tail, deleteMin(heap))
    }
  }

  property("correct deleteMin") = forAll { (heap: H) =>
    if (isEmpty(heap)) {
      Prop.throws(classOf[NoSuchElementException]) {
        deleteMin(heap)
      }
    } else {
      deleteMin(heap)
      true
    }

  }

  //read about 'whenever' clause to forAll, but it is in different mixin??
  property("minimum of two melded") = forAll { (heap1: H, heap2: H) =>
    (!isEmpty(heap1) && !isEmpty(heap2)) ==> {
      val totalMin = scala.math.min(findMin(heap1), findMin(heap2))
      totalMin.equals(findMin(meld(heap1, heap2)))
    }
  }

  property("repeated findMin should be monotonic") = forAll { (heap: H) =>
    isEmpty(heap) || {
      def checkMins(heap: H, prev: Int): Boolean = {
        if (isEmpty(heap)) true
        else {
          val currentMin = findMin(heap)
          prev <= currentMin && checkMins(deleteMin(heap), currentMin)
        }
      }

      checkMins(heap, findMin(heap))
    }
  }

}
