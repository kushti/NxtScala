package nxt.utils

import nxt._
import nxt.crypto.{EncryptedData, Crypto}
import scala.util.{Failure, Try}
import nxt.Appendix.{EncryptToSelfMessage, EncryptedMessage, Message}
import org.joda.time.DateTime


abstract class AbstractTransactionBuilder(attachment: Attachment, amount: Long) {
  type Self <: AbstractTransactionBuilder

  type MsgEither = Either[String, Array[Byte]]

  protected var plainMessage: Option[MsgEither] = None

  protected var nonDefaultFee: Option[Long] = None
  protected var recipient: Option[Long] = None
  protected var recipientPublicKey: Option[Array[Byte]] = None
  protected var referencedTransactionFullHash: Option[String] = None
  protected var deadline: Short = 1440

  protected var encryptedMessage: Option[Either[MsgEither, EncryptedMessage]] = None
  protected var encryptedMessageToSelf: Option[Either[MsgEither, EncryptToSelfMessage]] = None

  val publicKey: Array[Byte]
  val privateKey: Option[Array[Byte]]

  def withPublicMessage(message: String):Self = {
    plainMessage = Some(Left(message))
    this.asInstanceOf[Self]
  }

  def withPublicMessage(message: Array[Byte]) = {
    plainMessage = Some(Right(message))
    this.asInstanceOf[Self]
  }

  def withFee(txFee: Long) = {
    nonDefaultFee = Some(txFee)
    this.asInstanceOf[Self]
  }

  def withRecipient(recip: Long) = {
    recipient = Some(recip)
    this.asInstanceOf[Self]
  }

  def withPublicKeyAnnouncement(recip: Long, rcpPubKey: Array[Byte]) = {
    recipient = Some(recip)
    recipientPublicKey = Some(rcpPubKey)
    this.asInstanceOf[Self]
  }

  def withReferencedTransaction(referencedTxFullHash: String) = {
    this.referencedTransactionFullHash = Some(referencedTxFullHash)
    this.asInstanceOf[Self]
  }

  def withDeadline(ddl: Short) = {
    this.deadline = ddl
    this.asInstanceOf[Self]
  }

  def unsigned(): Try[Transaction] = Try {
    val fee = nonDefaultFee.getOrElse {
      attachment match {
        case _: Attachment.ColoredCoinsAssetIssuance => NxtFunctions.toNqt(1001)
        case _: Attachment.MessagingPollCreation => NxtFunctions.toNqt(11)
        case _ => Constants.ONE_NXT
      }
    }

    val tb = Nxt.getTransactionProcessor.newTransactionBuilder(publicKey, amount, fee, deadline, attachment)
    plainMessage.map { m => m match {
      case Left(s) => tb.message(new Message(s))
      case Right(ba) => tb.message(new Message(ba))
    }
    }

    recipient.map(r => tb.recipientId(r))
    recipientPublicKey.map(pk => tb.publicKeyAnnouncement(NxtFunctions.announcement(pk)))

    referencedTransactionFullHash.map(rt => tb.referencedTransactionFullHash(rt))

    encryptedMessage.map { emEither =>
      emEither match {
        case Left(s) =>
          val privKey = privateKey.get
          val theirPubOpt = recipientPublicKey.orElse(recipient.map(id => Account.getAccount(id).getPublicKey))

          theirPubOpt map { theirPub =>
            val (isText, bytes) = if (s.isLeft) (true, s.left.get.getBytes) else (false, s.right.get)
            val ed = EncryptedData.encrypt(bytes, privKey, theirPub)
            tb.encryptedMessage(new EncryptedMessage(ed, isText))
          }
        case Right(em) => tb.encryptedMessage(em)
      }
    }

    //todo: check
    encryptedMessageToSelf.map { emEither =>
      emEither match {
        case Left(s) =>
          val (isText, bytes) = if (s.isLeft) (true, s.left.get.getBytes) else (false, s.right.get)
          val ed = EncryptedData.encrypt(bytes, privateKey.get, publicKey)
          tb.encryptToSelfMessage(new EncryptToSelfMessage(ed, isText))

        case Right(em) => tb.encryptToSelfMessage(em)
      }
    }

    tb.build()
  }

  def signed(): Try[Transaction]

  def broadcastSigned(): Try[Transaction]
}


class PhraseTransactionBuilder(phrase: String, attachment: Attachment, amount: Long)
  extends AbstractTransactionBuilder(attachment, amount) {

  type Self = PhraseTransactionBuilder

  override val publicKey = Crypto.getPublicKey(phrase)
  override val privateKey = Some(Crypto.getPrivateKey(phrase))

  def withEncryptedMessage(msg: String) = {
    this.encryptedMessage = Some(Left(Left(msg)))
    this
  }

  def withEncryptedMessage(msg: Array[Byte]) = {
    this.encryptedMessage = Some(Left(Right(msg)))
    this
  }

  def withEncryptedMessageToSelf(msg: String) = {
    this.encryptedMessageToSelf = Some(Left(Left(msg)))
    this
  }

  def withEncryptedMessageToSelf(msg: Array[Byte]) = {
    this.encryptedMessageToSelf = Some(Left(Right(msg)))
    this
  }

  override def signed(): Try[Transaction] = unsigned().map { tx => tx.sign(phrase); tx}

  override def broadcastSigned() = signed().map { tx => Nxt.getTransactionProcessor.broadcast(tx); tx}
}

class PubKeyTransacionBuilder(pubKey: Array[Byte], attachment: Attachment, amount: Long)
  extends AbstractTransactionBuilder(attachment, amount) {

  type Self = PubKeyTransacionBuilder

  override val publicKey = pubKey
  override val privateKey = None

  def withEncryptedMessage(msg: EncryptedMessage) = {
    this.encryptedMessage = Some(Right(msg))
    this
  }

  def withEncryptedMessageToSelf(msg: EncryptToSelfMessage) = {
    this.encryptedMessageToSelf = Some(Right(msg))
    this
  }

  override def signed(): Try[Transaction] = throw new IllegalStateException("Can't sign transaction with just public key")

  override def broadcastSigned(): Try[Transaction] = signed()
}

//todo: use Cake Pattern for just tx bytes generation?
object TransactionTemplates {

  def issueTx(phrase: String, attachment: Attachment, amount: Long): Try[Transaction] =
    new PhraseTransactionBuilder(phrase, attachment, amount).broadcastSigned()

  def issueTx(phrase: String, attachment: Attachment): Try[Transaction] = issueTx(phrase, attachment, 0)

  def transferAssets(phrase:String, receiverId:Long, assetId:Long, qntAmount:Long, comment:String):Try[Transaction] = {
    val att = new Attachment.ColoredCoinsAssetTransfer(assetId, qntAmount)
    new PhraseTransactionBuilder(phrase, att, 0).withRecipient(receiverId).broadcastSigned()
  }

  def sendMoney(phrase: String, amount: Long, recipient: Long) =
    new PhraseTransactionBuilder(phrase, Attachment.ORDINARY_PAYMENT, amount)
      .withRecipient(recipient)
      .broadcastSigned()

  def sendMoney(phrase: String, amount: Long, recipient: Long, recipientPubKey: Array[Byte]) =
    new PhraseTransactionBuilder(phrase, Attachment.ORDINARY_PAYMENT, amount)
      .withPublicKeyAnnouncement(recipient, recipientPubKey)
      .broadcastSigned()


  def sendPublicMessage(phrase: String, text: String, recipient: Long) =
    new PhraseTransactionBuilder(phrase, Attachment.ARBITRARY_MESSAGE, 0)
      .withRecipient(recipient)
      .withPublicMessage(text)
      .broadcastSigned()

  def sendPublicMessage(phrase: String, text: String) =
    new PhraseTransactionBuilder(phrase, Attachment.ARBITRARY_MESSAGE, 0)
      .withPublicMessage(text)
      .broadcastSigned()


  def sendPublicMultipartMessage(phrase: String, text: String): Try[Transaction] = {
    val partSize = Constants.MAX_ARBITRARY_MESSAGE_LENGTH - 10
    if (text.getBytes.size > partSize * 10) Failure(new IllegalArgumentException("Too long text"))
    val parts = text.grouped(partSize).toList
    val headTxTry = sendPublicMessage(phrase, parts.head)

    headTxTry.flatMap{headTx =>
        parts.tail.foldLeft[Try[Transaction]](headTxTry) { case (tr, part) =>
          tr.map { prevTx =>
            val fh = prevTx.getFullHash
            new PhraseTransactionBuilder(phrase, Attachment.ARBITRARY_MESSAGE, 0)
              .withPublicMessage(part)
              .withReferencedTransaction(fh)
              .broadcastSigned()
          }.flatten
        }.map{_ => headTx}
    }
  }

  def publicKeyAnnouncement(phrase: String, senderPhrase: String) = {
    val pk = Crypto.getPublicKey(phrase)
    val rcpId = Account.getId(pk)

    new PhraseTransactionBuilder(senderPhrase, Attachment.ARBITRARY_MESSAGE, 0)
      .withPublicKeyAnnouncement(rcpId, pk)
      .broadcastSigned()
  }

  def checkThenFixPubKey(phrase: String, senderPhrase: String):Option[Try[Transaction]] = {
    if (Option(NxtFunctions.addOrGetAccount(phrase).getPublicKey).isEmpty) {
      Some(publicKeyAnnouncement(phrase, senderPhrase))
    } else None
  }

  def sellAlias(phrase:String, alias:String, priceNqt:Long, buyerIdOpt:Option[Long]):Try[Transaction] = {
    val att = new Attachment.MessagingAliasSell(alias, priceNqt)
    val ptb = new PhraseTransactionBuilder(phrase, att, 0)
    buyerIdOpt.map(rcp => ptb.withRecipient(rcp)).getOrElse(ptb).broadcastSigned()
  }

  def transferAlias(phrase:String, alias:String, recipientId:Long) = sellAlias(phrase, alias, 0, Some(recipientId))

  def buyAlias(phrase:String, alias:String, priceNqt:Long, sellerId:Long): Try[Transaction] ={
    val att = new Attachment.MessagingAliasBuy(alias)
    new PhraseTransactionBuilder(phrase, att, priceNqt).withRecipient(sellerId).broadcastSigned()
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