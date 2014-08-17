package nxt.utils

import nxt.{Block, BlockchainListener, Nxt}
import scala.annotation.tailrec
import scala.concurrent.{Await, Promise, Future}
import scala.concurrent.duration._


object WaitingUtils {

  //todo: less mutable approach :)
  lazy val listener = new BlockchainListener {
    start()

    var tasks = List[(Int,Promise[Unit])]()

    def submit(blocksToWait:Int):Future[Unit] = {
      val p = Promise[Unit]()
      tasks = height() + blocksToWait -> p :: tasks
      p.future
    }

    def notify(b: Block){
      tasks.filter(_._1==b.getHeight).foreach{case (_,p)=>
        p success Unit
      }
      tasks = tasks filter(_._1>b.getHeight)
    }
  }

  def height() = Nxt.getBlockchain.getHeight

  def untilSome[T](max: Int)(fn:  => Option[T]): Option[T] = {
    @tailrec
    def step(stN: Int)(fn:  => Option[T]): Option[T] = {
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

  def afterNextBlocks[T](howMany:Int)(fn: => T):T = {
    val currentHeight = height()
    println(s"Current height: $currentHeight")
    val f = listener.submit(howMany)
    Await.result(f, howMany+ 30 seconds)
    fn
  }

  def afterBlock[T](fn:  => T) = afterNextBlocks(1)(fn)

  def skipBlocks(n:Int) = afterNextBlocks(n)(Some())
  def skipBlock() = skipBlocks(1)

  def skipUntil(targetHeight:Int) = skipBlocks(targetHeight-height())
}
