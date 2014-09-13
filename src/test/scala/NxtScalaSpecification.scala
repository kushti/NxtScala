package nxt.nxtscala.test

import org.scalatest.{BeforeAndAfter, FunSuite}
import java.util.Properties
import nxt.{NxtFunctions, Nxt}
import scala.util.Random

class NxtScalaSpecification extends FunSuite with BeforeAndAfter{
  before{
    val propsRes = getClass.getClassLoader.getResource("nxt-default.properties")
    System.setProperty("nxt-default.properties", propsRes.getFile)
    val props = new Properties()
    props.load(getClass.getClassLoader.getResourceAsStream("nxt.properties"))
    Nxt.init(props)
  }

  test("balance - non-negative"){
    assert(NxtFunctions.balanceNQT(Random.nextString(50)) >=0)
  }

  test("account - add or get"){
    NxtFunctions.addOrGetAccount(Random.nextString(50))
  }

  test("accountId - always defined"){
    assert(Option(NxtFunctions.accountId(Random.nextString(50))).isDefined)
  }

  test("asset balance - non-negative"){
    assert(NxtFunctions.getAssetBalance(Random.nextLong(), Random.nextLong()) >=0)
  }




  after{}

}
