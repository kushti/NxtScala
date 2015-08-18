package nxt.utils

import nxt._
import nxt.util.Logger

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try

object WaitingUtils {

  import NxtFunctions._

  class Task[T](promise: Promise[T], task: => T) {
    def complete = promise.success(task)
  }

  lazy val listener = new BlockchainListener {
    start()

    val tasks = TrieMap[Int, Seq[Task[_]]]()

    def submit[T](blocksToWait: Int, task: => T): Future[T] = {
      val p = Promise[T]()
      val h = currentHeight + blocksToWait
      tasks.put(h, tasks.getOrElse(h, Seq()) :+ new Task(p, task))
      p.future
    }

    def notify(b: Block) {
      Logger.logDebugMessage(s"Block ${b.getHeight}, transactions inside: " + b.getTransactions.size)
      tasks.remove(b.getHeight).getOrElse(Seq()).foreach(_.complete)
    }
  }

  def generateBlock(forgerSecretPhrase: String) = Try {
    BlockchainProcessorImpl.getInstance.generateBlock(forgerSecretPhrase, Nxt.getEpochTime)
  }.recover { case e: BlockchainProcessor.BlockNotAcceptedException =>
    e.printStackTrace
  }

  def generateBlocks(forgerSecretPhrase: String,
                     howMany: Int) = (1 to howMany).foreach { _ => generateBlock(forgerSecretPhrase) }

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

  def afterNextBlocks[T](howMany: Int)(fn: => T): Future[T] = {
    Logger.logDebugMessage(s"Going to wait for $howMany blocks, current height is " + currentHeight)
    listener.submit(howMany, fn)
  }

  private def emptyFn = () => Unit

  def afterBlock[T](fn: () => T) = afterNextBlocks(1)(fn)

  def skipBlocks(n: Int) = afterNextBlocks(n)(emptyFn)

  def skipBlock() = skipBlocks(1)

  def skipUntil(targetHeight: Int) = skipBlocks(targetHeight - currentHeight)

  //sync
  def afterNextBlocksSync[T](howMany: Int)(fn: => T)(implicit atMost: Duration): T =
    Await.result(afterNextBlocks(howMany)(fn), atMost)

  def afterBlockSync[T](fn: () => T)(implicit atMost: Duration) = afterNextBlocksSync(1)(fn)

  def skipBlocksSync(n: Int)(implicit atMost: Duration) = afterNextBlocksSync(n)(emptyFn)

  def skipBlockSync()(implicit atMost: Duration) = skipBlocksSync(1)

  def skipUntilSync(targetHeight: Int)(implicit atMost: Duration) = skipBlocksSync(targetHeight - currentHeight)
}
