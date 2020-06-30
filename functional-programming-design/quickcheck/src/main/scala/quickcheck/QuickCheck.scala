package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.{Failure, Success, Try}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      heap <- genHeap
    } yield insert(i, heap)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("finding min of heap with after inserting 2 elements into an empty heap") = forAll { (a: Int, b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    findMin(meld(h1,h2)) == min(a,b)
  }

  property("insert and then deleting from a empty heap") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("finding min after melding two heaps") = forAll { (h1: H, h2: H) =>
    val min1 = Try(findMin(h1)).getOrElse(Int.MinValue)
    val min2 = Try(findMin(h2)).getOrElse(Int.MinValue)
    val mh = meld(h1,h2)

    Try(findMin(mh)) match {
      case Success(value) => value == min1 || value == min2
      case Failure(e) => e match {
        case ex:NoSuchElementException => true
        case _ => false
      }
    }
  }

  property("continuous finding and deleting min from a heap") = forAll { h: H =>
    val elems = extractFromHeap(h, Nil)
    checkSorted(elems)
  }

  property("continuous finding and deleting min from a heap: heap input inserted in order") = forAll(Gen.choose(0, 100)) { (n: Int) =>
    val inputList = (1 to n).toList
    val heap = inputList.foldLeft(empty)( (acc,x) =>  insert(x,acc) )
    extractFromHeap(heap, Nil) == inputList
  }

  private def checkSorted(elems: List[A]) : Boolean = {
    if(elems.isEmpty) true
    else
      elems.zip(elems.tail).forall { case (x,y) => x <= y}
  }

  private def extractFromHeap(h: H, acc: List[A]): List[A] = {
    if(isEmpty(h)) acc
    else{
      extractFromHeap(deleteMin(h), acc :+ findMin(h))
    }

  }
  private def min(a:Int, b:Int) = if(a < b) a else b
}
