package nxt

sealed trait AbstractMessage {
  val bytes: Array[Byte]
}

case class TextMessage(override val bytes:Array[Byte]) extends AbstractMessage{
  lazy val text = new String(bytes, "UTF-8")
}

case class BinaryMessage(override val bytes:Array[Byte]) extends AbstractMessage