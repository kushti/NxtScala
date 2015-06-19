package nxt

import nxt.util.Logger
import scala.util.Try
import java.sql.ResultSet
import resource._

class TransactionQueryBuilder {

  protected val sql = "SELECT * FROM transaction WHERE amount >= 0 "

  private def withSuffix(suffix:String) = {
    val s = this.sql
    new TransactionQueryBuilder{
      override protected val sql = s"$s $suffix "
    }
  }

  def withSender(accountId:Long) = withSuffix(s"AND sender_id = $accountId")

  def withRecipient(accountId:Long) = withSuffix(s"AND recipient_id = $accountId")

  def withId(id:Long) = withSuffix(s"AND id = $id")

  def withHeightMoreThan(height:Int) = withSuffix(s"AND height > $height")

  def withHeightLessThan(height:Int) = withSuffix(s"AND height < $height")

  def withPlainMessage() = withSuffix("AND has_message = true")

  def withType(txType:Byte) = withSuffix(s"AND type = $txType")

  def withType(txType:Byte, subType:Byte) = withSuffix(s"AND type = $txType AND subtype = $subType")

  def withReferenceToTransaction(tx:Transaction) = withSuffix(s"AND referenced_transaction_full_hash = '${tx.getFullHash}'")

  def query():Try[Seq[Transaction]] = {
    Logger.logDebugMessage(s"Going to execute query: $sql")
    Try{
      managed(Db.db.getConnection).map {con=>
        val pstmt = con.prepareStatement(sql)
        val rs: ResultSet = pstmt.executeQuery
        new Iterator[Transaction] {
          def hasNext = rs.next()
          def next() = TransactionDb.loadTransaction(con,rs)
        }.toList
      }.opt.get
    }
  }
}