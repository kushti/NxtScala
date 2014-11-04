package nxt

import java.io.{FileReader, File}
import java.util.Properties
import scala.annotation.tailrec
import scala.concurrent.duration._
import nxt.NxtFunctions._
import nxt.utils.TransactionTemplates._
import nxt.utils.WaitingUtils._


object LaunchingFunctions {

  def launch(): Unit = launch(None)

  def launch(confDirOpt:Option[String]): Unit = {
    val props = new Properties()

    confDirOpt match {
      case Some(confDir) =>
        System.setProperty("nxt-default.properties",confDir+"/nxt-default.properties")

        val propsFile = new File(confDir+"/nxt.properties")
        if(propsFile.exists()){
          props.load(new FileReader(propsFile))
        }

      case None =>
          val propsRes = getClass.getClassLoader.getResource ("nxt-default.properties")
          System.setProperty ("nxt-default.properties", propsRes.getFile)
          Option (getClass.getClassLoader.getResourceAsStream ("nxt.properties") ).map (props.load)
    }

      Nxt.init(props)
  }

  def waitForDownloadingDone(){
    @tailrec
    def downloadStep():Unit = {
      val lfh = NxtFunctions.lastFeederHeight
      val ch = NxtFunctions.currentHeight

      println(s"Last feeder height: $lfh, local height: $ch, isOffline: ${Constants.isOffline}")

      if ( !(Constants.isOffline || ((lfh!=0) && (lfh - ch) < 2))){
        println("Still downloading blockchain...")
        Thread.sleep(5000)
        downloadStep()
      }
    }
    downloadStep()
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
