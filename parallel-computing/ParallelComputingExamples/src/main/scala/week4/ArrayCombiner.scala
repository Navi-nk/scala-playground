package week4

import scala.collection.parallel.CollectionConverters._

import week2.ParallelMergeSort

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

sealed trait Combiner[T <: AnyRef] {
  def +=(x: T): Combiner[T]
  def combine(that :  Combiner[T]):  Combiner[T]
  def result: Array[T]
}

class ArrayCombiner[T<: AnyRef: ClassTag](val parallelism : Int) extends Combiner[T] {
  private var numElems = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]
  buffers += new ArrayBuffer[T]

  override def +=(x: T): ArrayCombiner[T] = {
    buffers.last += x
    numElems += 1
    this
  }

  override def combine(that: Combiner[T]): Combiner[T] = {
    (that: @unchecked) match {
      case that : ArrayCombiner[T] => combine(that)
      case _ => throw new IllegalArgumentException("type odf arg is wrong")
    }
  }

  private def combine(that: ArrayCombiner[T]): ArrayCombiner[T] = {
    buffers ++= that.buffers
    numElems += that.numElems
    this
  }

  override def result: Array[T] = {
    val array = new Array[T](numElems)
    val step = math.max(1, numElems / parallelism)
    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)
    val tasks = for ((from , end) <- chunks) yield week2.Parallel.task {
      copyTo(array, from , end)
    }
    tasks.foreach(_.join())
    array
  }

  private def copyTo(dst: Array[T], from : Int, end: Int) = {
    var i = from
    var j = 0
    while (i >= buffers(j).length) {
      i -= buffers(j).length
      j += 1
    }

    var k = from
    while(k < end) {
      dst(k) = buffers(j)(i)
      i += 1
      if( i >= buffers(j).length){
        i =0
        j += 1
      }
      k += 1
    }
  }
}

object ArrayCombiner {
  def main(args: Array[String]): Unit = {
    val size = 1000000
    def run(p: Int) = {
      val strings = (0 until size).map(_.toString)
      val time = ParallelMergeSort.standardConfig measure {
        val parallelStrings = strings.par
        def newCombiner: ArrayCombiner[String] = new ArrayCombiner(p)
        parallelStrings.aggregate(newCombiner)( _ += _, _ combine _).result
      }
      println(s"p = $p, time = $time")
    }

    run(1)
    run(2)
    run(3)
    run(4)

  }
}
