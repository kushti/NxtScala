package nxt

import scala.util.Try
import java.sql.{ResultSet, Connection}

class TransactionQueryBuilder {
  val sql:String = "SELECT * FROM transaction WHERE amount >= 0 "

  private def withPrefix(prefix:String) = new TransactionQueryBuilder{
    override val sql:String = s"$sql $prefix "
  }

  def withHeightMoreThan(height:Int) = withPrefix(s"AND height > $height")


  def withHeightLessThan(height:Int) = withPrefix(s"AND height < $height")

  def withPlainMessage() = withPrefix("AND has_message = true")

  def withType(txType:Byte) = withPrefix(s"AND type = $txType")

  def withType(txType:Byte, subType:Byte) = withPrefix(s"AND type = $txType AND subtype = $subType")

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