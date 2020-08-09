import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable

def power(x: Int , p:Double) = math.exp(p * math.log(math.abs(x))).toInt

def sumSegment(a: Array[Int], p: Double, s:Int, t: Int) : Int = {
  var i=s
  var sum:Int = 0
  while(i<t) {
    sum = sum + power(a(i), p)
    i += 1
  }
  sum
}


def pNorm(a: Array[Int], p:Double): Int = {
  power(sumSegment(a,p,0,a.length), p)
}

pNorm(Array(1,2,3,4,5), 2)

val forkJoinPool = new ForkJoinPool

abstract class TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B)
}

val scheduler =
  new DynamicVariable[TaskScheduler](new TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = schedule {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  })

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  scheduler.value.parallel(taskA, taskB)
}

def task[T](body: => T): ForkJoinTask[T] = {
  scheduler.value.schedule(body)
}


val threshold = 5
def segmentRec(a:Array[Int], p: Double, s: Int, t: Int): Int = {
  if(t - s < threshold)
    sumSegment(a, p, s, t)
  else {
    val m = s + ( t - s ) / 2
    val (sum1 , sum2) = parallel(segmentRec(a, p, s, m),
      segmentRec(a, p, m, t))
    sum1 + sum2
  }
}

def pNormRec(a: Array[Int], p: Double) = {
  power(segmentRec(a, p, 0 , a.length), p)
}

pNormRec(Array(1,2,3,4,5, 6, 7 ,8, 9, 10), 2)