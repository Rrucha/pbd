package tests

import org.scalatest._
import pbd.PaleBlueDot
import pbd.PaleBlueDot.whereToMeet

class ApplicationObjective extends FunSuite {

  val citiesFilename: String = "data/cities.csv"

  test("test 1"){
    val delhi: List[String] = List("in","delhi","07")
    val gandhinagar : List[String] = List("in","gandhinagar","09")
    val jodhpur : List[String] = List("in","jodhpur","24")
    val sarwar : List[String] = List("in", "sarwar", "24")
    val sikar : List[String] =List("in","sikar","24")

    assert( PaleBlueDot.whereToMeet(citiesFilename, delhi,delhi) == delhi )
    assert( PaleBlueDot.whereToMeet(citiesFilename, delhi,gandhinagar) == sarwar)
    assert(PaleBlueDot.whereToMeet(citiesFilename, delhi,jodhpur) == sikar)
  }

}
