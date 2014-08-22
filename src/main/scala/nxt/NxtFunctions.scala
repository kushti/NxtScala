package nxt

import nxt.crypto.Crypto
import nxt.Appendix.PublicKeyAnnouncement
import nxt.util.Convert

object NxtFunctions {

  def height = Nxt.getBlockchain.getHeight

  def balanceNQT(id:Long):Long =
    Option(Account.getAccount(id)).map(_.getGuaranteedBalanceNQT(Constants.CONFIRMATIONS_RELIABLE_TX)).getOrElse(0)

  def balanceNQT(phrase:String):Long = balanceNQT(Account.getId(Crypto.getPublicKey(phrase)))
  def balanceNQT(phrase:String*):Long = phrase.map(balanceNQT).sum

  def toNqt(nxt:Long) = nxt * Constants.ONE_NXT
  def toNxt(nqt:Long) = nqt / Constants.ONE_NXT

  def balanceNXT(id:Long):Long = toNxt(balanceNQT(id))
  def balanceNXT(phrase:String):Long = toNxt(balanceNQT(phrase))
  def balanceNXT(phrase:String*):Long = toNxt(balanceNQT(phrase:_*))

  def getAssetBalance(accountId:Long, assetId:Long):Long =
    Option(Account.getAccount(accountId)).map(_.getAssetBalanceQNT(assetId)).getOrElse(0L)

  //as class constructor has package-wide visibility
  def announcement(pubKey:Array[Byte]) = new PublicKeyAnnouncement(pubKey)

  //as method Account.addOrGetAccount has package-wide visibility
  def addOrGetAccount(phrase:String):Account = addOrGetAccount(Account.getId(Crypto.getPublicKey(phrase)))
  def addOrGetAccount(accountId:Long):Account = Account.addOrGetAccount(accountId)
}
