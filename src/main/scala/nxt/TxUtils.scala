package utils

import nxt._
import nxt.crypto.Crypto
import nxt.util.Convert
import nxt.Attachment.{MessagingPollCreation, ColoredCoinsAssetIssuance}
import scala.Some
import scala.util.Try


object TxUtils {
  val Fee = Constants.ONE_NXT
  val defaultDeadline:Short = 1440


  def issueTx(phrase: String, recipient: Long, amount: Long, attachmentOpt: Option[Attachment]): Try[Transaction] = Try {
    val pubKey = Crypto.getPublicKey(phrase)

    val tx = attachmentOpt match {
      case Some(attachment) =>

        val fee = attachment match {
          case _: Attachment.ColoredCoinsAssetIssuance => Constants.ASSET_ISSUANCE_FEE_NQT
          case _: Attachment.MessagingPollCreation => Constants.POLL_FEE_NQT
          case _ => Fee
        }

        println(s"Going to issue transaction with attachment to" +
          s"${Convert.toUnsignedLong(recipient)} : ${attachment.getJSONObject}, fee is $fee")

        Nxt.getTransactionProcessor.newTransaction(defaultDeadline, pubKey, recipient, amount, fee, null, attachment)

      case None =>
        println(s"Going to issue transaction without attachment to ${Convert.toUnsignedLong(recipient)}")
        Nxt.getTransactionProcessor.newTransaction(defaultDeadline, pubKey, recipient, amount, Fee, null)
    }

    tx.sign(phrase)
    Nxt.getTransactionProcessor.broadcast(tx)
    tx
  }

  def issueTx(phrase: String, recipient: Long, attachmentOpt: Option[Attachment]) = issueTx(phrase, recipient, 0, attachmentOpt)

  def issueTxToGodId(phrase: String, attachmentOpt: Option[Attachment]) = issueTx(phrase, Genesis.CREATOR_ID, attachmentOpt)
}
