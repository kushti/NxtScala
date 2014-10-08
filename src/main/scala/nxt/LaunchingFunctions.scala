package nxt

import java.util.Properties
import java.lang.reflect._
import scala.concurrent.duration._
import nxt.NxtFunctions._
import nxt.utils.TxUtils._
import nxt.utils.WaitingUtils._

object LaunchingFunctions {
  private def setFinalStatic(field:Field, newValue: Any){
    field.setAccessible(true)

    val modifiersField = classOf[Field].getDeclaredField("modifiers")
    modifiersField.setAccessible(true)
    modifiersField.setInt(field, field.getModifiers & ~Modifier.FINAL)

    field.set(null, newValue)
  }

  def launch(): Unit = {
      val propsRes = getClass.getClassLoader.getResource("nxt-default.properties")
      System.setProperty("nxt-default.properties", propsRes.getFile)
      val props = new Properties()
      Nxt.init(props)
  }

  def launchAndForge(forgingPhrase:String){
    val propsRes = getClass.getClassLoader.getResource("nxt-default.properties")
    System.setProperty("nxt-default.properties", propsRes.getFile)
    val props = new Properties()
    Option(getClass.getClassLoader.getResourceAsStream("nxt.properties")).map(props.load)
    setFinalStatic(classOf[nxt.Constants].getField("DIGITAL_GOODS_STORE_BLOCK"), 0)
    Nxt.init(props)
    Generator.startForging(forgingPhrase)
  }

  //todo: better place  ??
  //todo: desc
  def fixPubkeysAndBalances(masterAccPhrase:String, targetBalances:Map[String, Long])(implicit perBlock:Duration) = {
    targetBalances.map{case (phrase, _) =>
      checkThenFixPubKey(phrase, masterAccPhrase)
    }
    skipBlockSync()

    targetBalances.map {case (phrase, targetBalance) =>
        val accId = accountId(phrase)
        val b = balanceNqt(accId)
        if(b < targetBalance){
          sendMoney(masterAccPhrase, targetBalance-b, accId)
        }
    }
    skipBlockSync()
  }
}
