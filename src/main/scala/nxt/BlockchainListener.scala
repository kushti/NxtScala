package nxt

import nxt.util.Listener
import nxt.BlockchainProcessor.Event

trait BlockchainListener extends Listener[Block] {
  def notify(b: Block)

  def start(eventsToListen: Seq[Event]) {
    val bi = BlockchainProcessorImpl.getInstance
    eventsToListen foreach (ev => bi.addListener(this, ev))
  }

  def start() {
    start(Seq(BlockchainProcessor.Event.AFTER_BLOCK_APPLY))
  }
}