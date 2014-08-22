package nxt

import scala.collection.JavaConversions._

case class AssetOwnersInfo(assetId:Long, issuer:Long, totalAssets:Long, issuerAssets:Long, ownership:Map[Long,Long])

object AssetFunctions {

  //todo: totally ineffective method, should be fixed after 1.3.0
  def assetOwners(assetId:Long):AssetOwnersInfo = {
    val asset = Asset.getAsset(assetId)
    val issuer = asset.getAccountId
    val total = asset.getQuantityQNT

    val ownership = Account.getAllAccounts.flatMap{acc=>
      val balance = acc.getAssetBalanceQNT(assetId)
      if(balance>0 && acc.getId!=issuer) Some(acc.getId.toLong -> balance) else None
    }.toSeq.toMap

    val issuerAssets = Account.getAccount(issuer).getAssetBalanceQNT(assetId)

    AssetOwnersInfo(assetId, issuer, total, issuerAssets, ownership)
  }
}
