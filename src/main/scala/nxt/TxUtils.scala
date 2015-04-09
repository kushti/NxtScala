package nxt.utils

import nxt.Appendix.{Message, PublicKeyAnnouncement}
import nxt._
import nxt.crypto.Crypto
import org.joda.time.DateTime
import scala.util.Try


object TransactionTemplates {
  val DefaultDeadline: Short = 1440
  val DefaultFee = Constants.ONE_NXT

  def generateTxBuilder(phrase: String, attachment: Attachment,
                        amount: Long, fee: Long = DefaultFee): Transaction.Builder = {
    Nxt.newTransactionBuilder(Crypto.getPublicKey(phrase), amount, fee, DefaultDeadline, attachment)
  }

  def generateTx(phrase: String, attachment: Attachment, amount: Long, fee: Long = DefaultFee): Try[Transaction] =
    Try(generateTxBuilder(phrase, attachment, amount, fee).build(phrase))

  def generateTx(phrase: String, attachment: Attachment): Try[Transaction] = generateTx(phrase, attachment, 0)

  private def broadcastAndReturn(tx: Transaction) = {
    Nxt.getTransactionProcessor.broadcast(tx)
    tx
  }

  def transferAssets(phrase: String, recipient: Long, assetId: Long,
                     qntAmount: Long, comment: String): Try[Transaction] = Try {
    val att = new Attachment.ColoredCoinsAssetTransfer(assetId, qntAmount)
    val tx = generateTxBuilder(phrase, att, 0).recipientId(recipient).build(phrase)
    broadcastAndReturn(tx)
  }

  def sendMoney(phrase: String, amount: Long, recipient: Long) = Try {
    val tx = generateTxBuilder(phrase, Attachment.ORDINARY_PAYMENT, amount).recipientId(recipient).build(phrase)
    broadcastAndReturn(tx)
  }

  def sendMoney(phrase: String, amount: Long, recipient: Long, recipientPubKey: Array[Byte]) = Try {
    val tx = generateTxBuilder(phrase, Attachment.ORDINARY_PAYMENT, amount)
      .recipientId(recipient)
      .publicKeyAnnouncement(new PublicKeyAnnouncement(recipientPubKey))
      .build(phrase)
    broadcastAndReturn(tx)
  }

  def sendPublicMessage(phrase: String, text: String, recipient: Long = 0) = Try {
    val tx = generateTxBuilder(phrase, Attachment.ARBITRARY_MESSAGE, 0)
      .recipientId(recipient)
      .message(new Message(text))
      .build(phrase)
    broadcastAndReturn(tx)
  }


  /*
  def sendPublicMultipartMessage(phrase: String, text: String): Try[Transaction] = {
    val partSize = Constants.MAX_ARBITRARY_MESSAGE_LENGTH - 10
    if (text.getBytes.size > partSize * 10) Failure(new IllegalArgumentException("Too long text"))
    val parts = text.grouped(partSize).toList
    val headTxTry = sendPublicMessage(phrase, parts.head)

    headTxTry.flatMap { headTx =>
      parts.tail.foldLeft[Try[Transaction]](headTxTry) { case (tr, part) =>
        tr.map { prevTx =>
          val fh = prevTx.getFullHash
          new PhraseTransactionBuilder(phrase, Attachment.ARBITRARY_MESSAGE, 0)
            .withPublicMessage(part)
            .withReferencedTransaction(fh)
            .broadcastSigned()
        }.flatten
      }.map { _ => headTx }
    }
  } */

  def publicKeyAnnouncement(phrase: String, senderPhrase: String) = {
    val pk = Crypto.getPublicKey(phrase)
    val rcpId = Account.getId(pk)

    sendMoney(phrase, 0, rcpId, pk)
  }

  def checkThenFixPubKey(phrase: String, senderPhrase: String): Option[Try[Transaction]] = {
    if (Option(NxtFunctions.addOrGetAccount(phrase).getPublicKey).isEmpty) {
      Some(publicKeyAnnouncement(phrase, senderPhrase))
    } else None
  }

  def sellAlias(phrase: String, alias: String, priceNqt: Long, buyerIdOpt: Option[Long]): Try[Transaction] = Try {
    val att = new Attachment.MessagingAliasSell(alias, priceNqt)

    val tb = generateTxBuilder(phrase, att, 0)
    val tx = buyerIdOpt.map(buyerId => tb.recipientId(buyerId)).getOrElse(tb).build(phrase)
    broadcastAndReturn(tx)
  }

  def transferAlias(phrase: String, alias: String, recipientId: Long) = sellAlias(phrase, alias, 0, Some(recipientId))

  def buyAlias(phrase: String, alias: String, priceNqt: Long, sellerId: Long): Try[Transaction] = Try {
    val att = new Attachment.MessagingAliasBuy(alias)
    val tx = generateTxBuilder(phrase, att, 0).recipientId(sellerId).build(phrase)
    broadcastAndReturn(tx)
  }
}

object TxSeqUtils {
  def withTextMessage(txs: Iterable[Transaction]): Iterable[Transaction] = txs flatMap {
    tx =>
      Option(tx.getMessage).flatMap(msgAppendix => if (!msgAppendix.isText) None else Some(tx))
  }

  def betweenTimestamps(txs: Iterable[Transaction], startTime: DateTime, endTime: DateTime) = txs.filter { tx =>
    val timestamp = tx.getTimestamp * 1000L + Constants.EPOCH_BEGINNING
    timestamp <= endTime.getMillis && timestamp >= startTime.getMillis
  }
}