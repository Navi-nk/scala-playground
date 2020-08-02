

class HelloThread extends Thread {

  override def run(): Unit = {
    val uids = for (i <- 0 until 10 ) yield (ThreadExample.getUniqueID , ThreadExample.getThreadSafeUniqueID)
    println("Non Thread safe:"+uids.map(_._1))
    println("Thread safe:"+uids.map(_._2))
  }
}

object ThreadExample {
  var uuid:Int = 0
  //Threads will have overlapping UUIDs - operation is not atomic
  def getUniqueID = {
    uuid = uuid + 1
    uuid
  }
  var uuid2:Int = 0
  def getThreadSafeUniqueID = synchronized {
    uuid2 = uuid2 + 1
    uuid2
  }


  def main(args: Array[String]): Unit = {
    val t1 = new HelloThread
    val t2 = new HelloThread
    t1.start()
    t2.start()
    t1.join()
    t2.join()
  }
}