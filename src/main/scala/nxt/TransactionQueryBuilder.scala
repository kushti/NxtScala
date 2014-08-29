package nxt

import scala.util.Try
import java.sql.{ResultSet, Connection}
import nxt.db.Db

class TransactionQueryBuilder {
  val sql = "SELECT * FROM transaction WHERE amount >= 0 "

  private def withTemplate(prefix:String) = new TransactionQueryBuilder{
    override val sql = s"$sql $prefix "
  }

  def withHeightMoreThan(height:Int) = withTemplate(s"AND height > $height")


  def withHeightLessThan(height:Int) = withTemplate(s"AND height < $height")

  def withPlainMessage() = withTemplate("AND has_message = true")

  def withType(txType:Byte) = withTemplate(s"AND type = $txType")

  def withType(txType:Byte, subType:Byte) =  new TransactionQueryBuilder{
    override val sql = s"$sql AND type = $txType AND subtype = $subType"
  }

  def query():Try[Seq[Transaction]] = Try{
    val con: Connection = Db.getConnection
    val pstmt= con.prepareStatement(sql)
    val rs: ResultSet = pstmt.executeQuery
    new Iterator[Transaction] {
      def hasNext = rs.next()
      def next() = TransactionDb.loadTransaction(con,rs)
    }.toSeq
  }
}