package nxt

import scala.collection.JavaConversions._
import NxtFunctions._




case class BlockHandler(block: Block) {
  def transactions = block.getTransactions

  def transactionsWithPlainMessage = block.getTransactions.filter(tx => Option(tx.getMessage).isDefined)

  def txsWithMsg = transactionsWithPlainMessage

  def transactionsWithEncryptedMessage = block.getTransactions.filter(tx => Option(tx.getEncryptedMessage).isDefined)

  def txsWithEncMsg = transactionsWithEncryptedMessage

  def transactionsWithEncryptedMessageToSelf =
    block.getTransactions.filter(tx => Option(tx.getEncryptToSelfMessage).isDefined)

  def txsWithEncMsgToSelf = transactionsWithEncryptedMessageToSelf

  def transactionsWithEncryptedMessagesFor(accId: Long): Seq[Transaction] = transactions.filter {
    tx =>
      (tx.getRecipientId.toLong == accId && Option(tx.getEncryptedMessage).isDefined) ||
        (tx.getSenderId.toLong == accId && Option(tx.getEncryptToSelfMessage).isDefined)
  }

  def toMessage(isText:Boolean, bytes:Array[Byte]) = isText match{
    case true => TextMessage(bytes)
    case false => BinaryMessage(bytes)
  }

  def decryptMessages(phrase: String): Seq[Message] = {
    val accId = accountId(phrase)
    Option(Account.getAccount(accId)).map {
      acc =>
        transactions.flatMap {
          tx => if (tx.getRecipientId.toLong == accId)
            Option(tx.getEncryptedMessage)
              .map(msg => toMessage(msg.isText, acc.decryptFrom(msg.getEncryptedData, phrase)))
          else if (tx.getSenderId.toLong == accId)
            Option(tx.getEncryptToSelfMessage)
              .map(msg => toMessage(msg.isText, acc.decryptFrom(msg.getEncryptedData, phrase)))
          else None
        }
    }.getOrElse(Seq())
  }
}
