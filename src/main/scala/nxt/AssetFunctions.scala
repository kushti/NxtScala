package nxt

import scala.collection.JavaConversions._
import nxt.util.Convert


case class Share(accountId: Long, shares:Long, percentage:Double){
  override def toString = s"Share(id: ${Convert.rsAccount(accountId)}, shares: $shares, %%: $percentage)"
}

case class AssetOwnersInfo(assetId:Long, height: Int, totalAssets:Long, issuerShare:Share, otherShares:Seq[Share]){

  override def toString = {
    s"Asset: ${Convert.toUnsignedLong(assetId)}, total assets: $totalAssets, " +
    s"height: $height, issuer assets: $issuerShare, other owners:"+otherShares.mkString("(",",",")")
  }
}

object AssetFunctions {

  //todo: totally ineffective method, should be fixed after 1.3.0
  def assetOwners(assetId:Long):AssetOwnersInfo = {
    val height = NxtFunctions.height
    val asset = Asset.getAsset(assetId)
    val issuer = asset.getAccountId
    val total = asset.getQuantityQNT

    val totalDouble = total.toDouble

    val issuerBalance = Account.getAccount(issuer).getAssetBalanceQNT(assetId)
    val issuerShare = Share(issuer, issuerBalance, issuerBalance/totalDouble)

    val otherShares = Account.getAllAccounts(0, Int.MaxValue).iterator().flatMap{acc=>
      val balance = acc.getAssetBalanceQNT(assetId)
      if(balance>0 && acc.getId!=issuer){
        Some(Share(acc.getId.toLong, balance, balance/totalDouble))
      } else None
    }.toSeq

    AssetOwnersInfo(assetId, height, total, issuerShare, otherShares)
  }
}
