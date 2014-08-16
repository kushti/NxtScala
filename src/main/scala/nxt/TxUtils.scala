package nxt.utils

import nxt._
import nxt.crypto.Crypto
import nxt.util.Convert
import nxt.Attachment.{EmptyAttachment, MessagingPollCreation, ColoredCoinsAssetIssuance}
import scala.Some
import scala.util.Try
import nxt.Appendix.{Message, PublicKeyAnnouncement}


object TxUtils {
  val Fee = Constants.ONE_NXT
  val defaultDeadline: Short = 1440


  def issueTx(phrase: String, attachment: Attachment, amount: Long,
              recipientOpt: Option[Long], rcpPubKeyOpt: Option[Array[Byte]],
              messageOpt:Option[String]): Try[Transaction] = Try {

    val pubKey = Crypto.getPublicKey(phrase)

    val fee = attachment match {
      case _: Attachment.ColoredCoinsAssetIssuance => Constants.ASSET_ISSUANCE_FEE_NQT
      case _: Attachment.MessagingPollCreation => Constants.POLL_FEE_NQT
      case _ => Fee
    }

    println(s"Going to issue transaction with attachment to $recipientOpt : ${attachment.getJSONObject}, fee is $fee")

    val tb = Nxt.getTransactionProcessor.newTransactionBuilder(pubKey, amount, fee, defaultDeadline, attachment)
    recipientOpt.map(rcp => tb.recipientId(rcp))
    rcpPubKeyOpt.map(pubKey => tb.publicKeyAnnouncement(NxtFunctions.announcement(pubKey)))
    messageOpt.map(message=> tb.message(new Message(message)))

    val tx = tb.build()

    tx.sign(phrase)
    Nxt.getTransactionProcessor.broadcast(tx)
    tx
  }

  def issueTx(phrase: String, attachment: Attachment, amount: Long, recipientOpt: Option[Long]): Try[Transaction] =
    issueTx(phrase, attachment, amount, recipientOpt, None, None)

  def issueTx(phrase: String, attachment: Attachment): Try[Transaction] = issueTx(phrase, attachment, 0, None)



  def sendMoney(phrase: String, amount:Long, recipient:Long) =
    issueTx(phrase, Attachment.ORDINARY_PAYMENT, amount, Some(recipient))

  def sendMessage(phrase:String, text:String, recipient:Long) =
    issueTx(phrase, Attachment.ARBITRARY_MESSAGE, 0, Some(recipient), None, Some(text))

  //todo: is it right?
  def publicKeyAnnouncement(phrase:String) = {
    val pk = Crypto.getPublicKey(phrase)
    val accId = Account.getId(pk)
    issueTx(phrase, Attachment.ORDINARY_PAYMENT, 1, Some(accId), Some(pk), None)
  }

  def checkThenFixPubKey(phrase:String) = {
    if(Option(NxtFunctions.addOrGetAccount(phrase).getPublicKey).isEmpty){
      publicKeyAnnouncement(phrase)
    }
  }
}
