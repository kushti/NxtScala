package nxt

import nxt.crypto.Crypto
import nxt.Appendix.PublicKeyAnnouncement
import nxt.util.Convert
import nxt.db.Db

object NxtFunctions {

  def withinDbTransaction[T](fn: => T):T={
    Db.beginTransaction()
    val t = fn
    Db.endTransaction()
    t
  }

  def height = Nxt.getBlockchain.getHeight

  def lastFeederHeight = Nxt.getBlockchainProcessor.getLastBlockchainFeederHeight

  def balanceNqt(id:Long):Long = Option(Account.getAccount(id)).map(_.getBalanceNQT).getOrElse(0)

  def balanceNqt(phrase:String):Long = balanceNqt(accountId(phrase))
  def balanceNqt(phrase:String*):Long = phrase.map(balanceNqt).sum

  def toNqt(nxt:Long) = nxt * Constants.ONE_NXT
  def toNxt(nqt:Long) = nqt / Constants.ONE_NXT

  def balanceNxt(id:Long):Long = toNxt(balanceNqt(id))
  def balanceNxt(phrase:String):Long = toNxt(balanceNqt(phrase))
  def balanceNxt(phrase:String*):Long = toNxt(balanceNqt(phrase:_*))

  def getAssetBalance(accountId:Long, assetId:Long):Long =
    Option(Account.getAccount(accountId)).map(_.getAssetBalanceQNT(assetId)).getOrElse(0L)

  //as class constructor has package-wide visibility
  def announcement(pubKey:Array[Byte]) = new PublicKeyAnnouncement(pubKey)

  //as method Account.addOrGetAccount has package-wide visibility
  def addOrGetAccount(phrase:String):Account = addOrGetAccount(Account.getId(Crypto.getPublicKey(phrase)))
  def addOrGetAccount(accountId:Long):Account = withinDbTransaction(Account.addOrGetAccount(accountId))

  def transactionById(id:Long) = Option(TransactionDb.findTransaction(id))

  def accountId(phrase:String) = Account.getId(Crypto.getPublicKey(phrase)).toLong
  def accountIds(phrases:Seq[String]):Seq[Long] = phrases map accountId

  def generateBlock(phrase:String) = {
    Nxt.getBlockchainProcessor.asInstanceOf[BlockchainProcessorImpl].generateBlock(phrase, Convert.getEpochTime)
  }

  def popOff(howMany:Int) = {
    BlockchainProcessorImpl.getInstance().popOffTo(NxtFunctions.height-howMany)
  }
}
