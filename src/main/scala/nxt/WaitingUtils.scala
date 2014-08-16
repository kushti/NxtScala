package nxt.utils

import nxt.{Block, BlockchainListener, Nxt}
import scala.annotation.tailrec


object WaitingUtils {
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

  def afterBlock[T](fn:  => Option[T]) = afterNextBlocks(1)(fn)

  //todo: make it non-blocking
  def afterNextBlocks[T](howMany:Int)(fn: => Option[T]):Option[T] = {
    val currentHeight = height()
    println(s"Current height: $currentHeight")

    var res:Option[T] = None

    new BlockchainListener {
      def notify(b: Block){
        if(b.getHeight - currentHeight == howMany){
          println("Block applied: "+b.getHeight)
          res = fn
        }
      }
    }

    Thread.sleep((howMany+5)*1000) //todo:fix
    res
  }

  def skipBlocks(n:Int) = afterNextBlocks(n)(Some())
  def skipBlock() = skipBlocks(1)

  def skipUntil(targetHeight:Int) = skipBlocks(targetHeight-height())
}
