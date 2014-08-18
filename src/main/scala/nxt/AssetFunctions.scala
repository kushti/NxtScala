package nxt

import scala.collection.JavaConversions._

case class AssetOwnersInfo(assetId:Long, issuer:Long, ownership:Map[Long,Long])

object AssetFunctions {

  //todo: totally ineffective method, should be fixed after 1.3.0
  def assetOwners(assetId:Long):AssetOwnersInfo = {
    val ownership:Map[Long,Long] = Account.getAllAccounts.flatMap{acc=>
      val balance = acc.getAssetBalanceQNT(assetId)
      if(balance>0) Some(acc.getId.toLong -> balance) else None
    }.toSeq.toMap

    val issuer = Asset.getAsset(assetId).getAccountId
    AssetOwnersInfo(assetId, issuer, ownership)
  }


}
