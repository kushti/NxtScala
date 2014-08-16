package nxt

import nxt.util.Listener

trait BlockchainListener extends Listener[Block] {
  val bi = BlockchainProcessorImpl.getInstance
  bi.addListener(this, BlockchainProcessor.Event.AFTER_BLOCK_APPLY)

  def notify(b:Block)
}