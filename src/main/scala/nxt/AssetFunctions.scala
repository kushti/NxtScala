package nxt

import scala.collection.JavaConversions._
import nxt.util.Convert

case class AssetOwnersInfo(assetId:Long, issuer:Long, totalAssets:Long, height: Int,
                           issuerAssets:Long, ownership:Map[Long,Long]){
  override def toString = {
    s"Asset: ${Convert.toUnsignedLong(assetId)}, issuer: ${Convert.rsAccount(issuer)}, total assets: $totalAssets, " +
    s"height: $height, issuer assets: $issuerAssets, other owners:"+ownership.map{case (k,v)=> Convert.rsAccount(k)->v}
  }
}

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
    val height = NxtFunctions.height

    AssetOwnersInfo(assetId, issuer, total, height, issuerAssets, ownership)
  }
}
