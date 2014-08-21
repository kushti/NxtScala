package nxt

import scala.collection.JavaConversions._

case class AssetOwnersInfo(assetId:Long, issuer:Long, totalAssets:Long, ownership:Map[Long,Long])

object AssetFunctions {

  //todo: totally ineffective method, should be fixed after 1.3.0
  def assetOwners(assetId:Long):AssetOwnersInfo = {
    val ownership = Account.getAllAccounts.flatMap{acc=>
      val balance = acc.getAssetBalanceQNT(assetId)
      if(balance>0) Some(acc.getId.toLong -> balance) else None
    }.toSeq.toMap

    val asset = Asset.getAsset(assetId)
    val issuer = asset.getAccountId
    val total = asset.getQuantityQNT
    AssetOwnersInfo(assetId, issuer, total, ownership)
  }


}
