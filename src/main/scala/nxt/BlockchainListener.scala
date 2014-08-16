package nxt

import nxt.util.Listener

trait BlockchainListener extends Listener[Block] {
  def notify(b:Block)

  def start(){
    val bi = BlockchainProcessorImpl.getInstance
    bi.addListener(this, BlockchainProcessor.Event.AFTER_BLOCK_APPLY)
  }
}