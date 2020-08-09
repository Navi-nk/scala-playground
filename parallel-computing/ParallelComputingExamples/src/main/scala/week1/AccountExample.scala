class Account(var amount: Int = 0) {
  //might result in deadlocks
  def transfer(target: Account, n: Int) = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
        println(s"Amount $n transfered. current amount: $amount, target amount ${target.amount}")
      }
    }
  }

  //In this case if two thread try to transfer to each other account then the
  // first thread will transfer in one direction a -> b in this case a is blocked and will try to block b
  // if same time second thread starts to transfer in other direction b -> a then it will try to block a but it is already blocked by thread 1
  // so b is kept up blocked so thread 1 acquires lock on b and transfer completes. now thread 2 is unblocked so the transfer from b -> a happens
  //Here a is given higher uuid then b so the blocking on a always happens before blocking on b
  val uid = AccountExample.getThreadSafeUniqueID
  def transferSafe(target: Account, n: Int) = {
    if (this.uid < target.uid) this.transfer(target, n)
    else target.transfer(this, -n)
  }
}


object AccountExample {

  var uuid:Int = 0
  def getThreadSafeUniqueID = synchronized {
    uuid = uuid + 1
    uuid
  }


  def startThread(a: Account, b: Account, n: Int, increments: Int): Thread = {
    val t = new Thread {
      override def run(): Unit = {
        for (_ <- 0 until n) a.transferSafe(b, increments)
      }
    }
    t.start()
    t
  }

  def main(args: Array[String]): Unit = {
    val a1 = new Account(500000)
    val a2 = new Account(700000)

    //Deadlock!!!
    val t = startThread(a1, a2, 1000, 1)
    val s = startThread(a2, a1, 1000, 2)
    t.join()
    s.join()

    println(s"final amounts - a : ${a1.amount} b : ${a2.amount}")
  }
}
