package nxt

import scala.util.Try
import java.sql.{ResultSet, Connection}
import nxt.db.Db

class TransactionQueryBuilder {
  val sql = "SELECT * FROM transaction WHERE amount >= 0 "

  //todo: reduce boilerplate

  def heightMoreThan(height:Int) = new TransactionQueryBuilder{
    override val sql = s"$sql AND height > $height "
  }

  def heightLessThan(height:Int) = new TransactionQueryBuilder{
    override val sql = s"$sql AND height < $height "
  }

  def havingMessage() = new TransactionQueryBuilder{
    override val sql = s"$sql AND has_message = true "
  }

  def withType(txType:Byte) =  new TransactionQueryBuilder{
    override val sql = s"$sql AND type = $txType"
  }

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