package week3

import scala.collection._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParSeq, ParSet}


object DataParallel {
  def isVowel(c: Char) = {
    Array('a', 'e', 'i', 'o', 'u','A','E','I','O','U').contains(c)
  }

  def palindrome(xs :ParSeq[Int]): List[Int] = {
    xs.aggregate(List[Int]())(
      (agg, n) =>  if(n.toString == n.toString.reverse) agg :+ n else agg,
      _ ++ _
    )
  }

  def intersection(a: GenSet[Int], b:GenSet[Int]) = {
    val result = mutable.Set[Int]()
    for(x <- a) if(b contains x) result += x
    result
  }

  //Wrong!!!!! result var gets corrupted by multiple threads
  def intersection(a: ParSet[Int], b:ParSet[Int]) = {
    val result = mutable.Set[Int]()
    for(x <- a) if(b contains x) result += x
    result
  }

  def intersectionSafe(a: ParSet[Int], b:ParSet[Int]): ParSet[Int] = {
    if(a.size > b.size) a.filter(b(_))
    else b.filter(a(_))
  }


  def main(args: Array[String]): Unit = {
    val out = Array('E', 'P', 'F', 'L').par.aggregate(0)(
      (count, c) => if (isVowel(c)) count + 1 else count,
      _ + _
    )
    println(out)

    val array = (0 to 10000).toArray
    println(palindrome(array.par))

    val seqRes = intersection((0 to 1000).toSet, (0 to 1000 by 4).toSet)
    val parRes = intersection((0 to 1000).par.toSet, (0 to 1000 by 4).par.toSet)
    val parResSafe = intersectionSafe((0 to 1000).par.toSet, (0 to 1000 by 4).par.toSet)
    println(seqRes.size == parRes.size) //false most of the times
    println(seqRes.size == parResSafe.size) //true


    //0->1 1->2 2->3 3->4 .... N->0
    val graph = mutable.Map[Int, Int]()
    graph ++= (0 until 100000).map(i => (i , i+1))
    graph(graph.size - 1) = 0

    //This is wrong!!! concurrent writes
    //0->2 1->3 2->4 .... N->2
    for((k,v) <- graph.par) graph(k) = graph(v)

    val violation = graph.find( { case (i,v) => v != (i + 2) % graph.size})
    println(violation) //should be empty but will not br


    //One of correct way of doing the same
    val graph1 = concurrent.TrieMap[Int,Int]() ++= (0 until 100000).map(i => (i , i+1))
    graph1(graph1.size - 1) = 0

    val previous = graph1.snapshot()
    for((k,v) <- graph1.par) graph1(k) = previous(v)

    val violation1 = graph1.find( { case (i,v) => v != (i + 2) % graph1.size})
    println(violation1) //should be empty


  }



}
