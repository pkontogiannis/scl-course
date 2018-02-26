package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insertTwoElemsIntoEmptyHeap") = forAll((a1: Int, a2: Int) => {
    val h = insert(a1, insert(a2, empty))
    val smallest = if (a1 < a2) a1 else a2 //(a1<a2) ? a1: a2
    findMin(h) == smallest
  })

  property("insertAndDeleteElementFromEmptyHeap") = forAll((a: Int) => {
    val h = insert(a, empty)
    deleteMin(h)
    isEmpty(h)
  })

  property("isSorted") = forAll((h: H) => {
    def deleteMinimal(hp: H, l: List[Int]): List[Int] = {
      if (isEmpty(h))
        l
      else {
        findMin(h) :: deleteMinimal(deleteMin(h), l)
      }
    }

    val x = deleteMinimal(h, Nil)
    x == x.sorted
  })

  property("minimumOfMelding") = forAll((h1: H, h2: H) => {
    //    min == oneOf(findMin(h1), findMin(h2))
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    findMin(meld(h1, h2)) == (if (minH1 < minH2) minH1 else minH2)
  })

}
