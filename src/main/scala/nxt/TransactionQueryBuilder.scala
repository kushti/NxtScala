package nxt

import scala.util.{Failure, Success, Try}
import java.sql.{ResultSet, Connection}
import resource._

class TransactionQueryBuilder {

  protected val sql = "SELECT * FROM transaction WHERE amount >= 0 "

  private def withPrefix(prefix:String) = {
    val s = this.sql
    new TransactionQueryBuilder{
      override protected val sql = s"$s $prefix "
    }
  }

  def withSender(accountId:Long) = withPrefix(s"AND sender_id = $accountId")

  def withId(id:Long) = withPrefix(s"AND id = $id")

  def withHeightMoreThan(height:Int) = withPrefix(s"AND height > $height")

  def withHeightLessThan(height:Int) = withPrefix(s"AND height < $height")

  def withPlainMessage() = withPrefix("AND has_message = true")

  def withType(txType:Byte) = withPrefix(s"AND type = $txType")

  def withType(txType:Byte, subType:Byte) = withPrefix(s"AND type = $txType AND subtype = $subType")

  def query():Try[Seq[Transaction]] = {
    println(s"Going to execute query: $sql")
    Try{
      managed(Db.getConnection).map {con=>
        val pstmt = con.prepareStatement(sql)
        val rs: ResultSet = pstmt.executeQuery
        new Iterator[Transaction] {
          def hasNext = rs.next()
          def next() = TransactionDb.loadTransaction(con,rs)
        }.toSeq
      }.opt.get
    }
  }
}


//todo: better name
class BlockDbWrapper{
  def getBlock(id:Long):Option[BlockImpl] = Option(BlockDb.findBlock(id))
}