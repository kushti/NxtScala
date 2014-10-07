package nxt.utils

import nxt._
import scala.annotation.tailrec
import scala.concurrent.{Await, Promise, Future}
import scala.concurrent.duration._
import scala.collection.concurrent.TrieMap
import scala.util.Random


object FastBlocksGenerator{
  private def runInAnotherThread(fn: =>Unit): Unit = new Thread(new Runnable() {
    override def run() {
      while (true) {
        fn
      }
    }
  }).start()

  private def simple(generatorPhrase:String) : Unit = {
    Thread.sleep(1250)
    NxtFunctions.generateBlock(generatorPhrase)
  }

  def runSimple(generatorPhrase:String) = runInAnotherThread(simple(generatorPhrase))

  def runWithPopOffs(generatorPhrase:String) = {
    runSimple(generatorPhrase)
    if(Random.nextInt(10)==7){
      NxtFunctions.popOff(Random.nextInt(2)+1)
    }
  }
}

object WaitingUtils {
  class Task[T](promise:Promise[T], task: => T){
    def complete = promise.success(task)
  }

  lazy val listener = new BlockchainListener {
    start()

    val tasks = TrieMap[Int, Seq[Task[_]]]()

    def submit[T](blocksToWait: Int, task: => T): Future[T] = {
      val p = Promise[T]()
      val h = height() + blocksToWait
      tasks.put(h, tasks.getOrElse(h, Seq()) :+ new Task(p,task))
      p.future
    }

    def notify(b: Block) {
      println(s"Block ${b.getHeight}, transactions inside: " + b.getTransactions.size)
      tasks.remove(b.getHeight).getOrElse(Seq()).foreach(_.complete)
    }
  }

  def height() = Nxt.getBlockchain.getHeight

  def untilSome[T](max: Int)(fn: => Option[T]): Option[T] = {
    @tailrec
    def step(stN: Int)(fn: => Option[T]): Option[T] = {
      fn match {
        case res: Some[T] => res
        case None =>
          if (stN == max)
            None
          else
            step(stN + 1)(fn)
      }
    }
    step(0)(fn)
  }

  def afterNextBlocks[T](howMany: Int)(fn:  => T): Future[T] = {
    println(s"Going to wait for $howMany blocks, current height is " + height())
    listener.submit(howMany, fn)
  }

  private def emptyFn = () => Unit

  def afterBlock[T](fn: () => T) = afterNextBlocks(1)(fn)
  def skipBlocks(n: Int) = afterNextBlocks(n)(emptyFn)
  def skipBlock() = skipBlocks(1)
  def skipUntil(targetHeight: Int) = skipBlocks(targetHeight - height())

  //sync
  def afterNextBlocksSync[T](howMany: Int)(fn: => T)(implicit atMost:Duration): T =
    Await.result(afterNextBlocks(howMany)(fn), atMost)

  def afterBlockSync[T](fn: () => T)(implicit atMost:Duration) = afterNextBlocksSync(1)(fn)
  def skipBlocksSync(n: Int)(implicit atMost:Duration) = afterNextBlocksSync(n)(emptyFn)
  def skipBlockSync()(implicit atMost:Duration) = skipBlocksSync(1)
  def skipUntilSync(targetHeight: Int)(implicit atMost:Duration) = skipBlocksSync(targetHeight - height())
}
