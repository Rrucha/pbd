package tests

import org.scalatest._
import pbd.PaleBlueDot
class LectureObjective2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"

  test("test 1") {

    assert(PaleBlueDot.getCountryCode(countriesFile, "india") == "in", "india all small")
    assert(PaleBlueDot.getCountryCode(countriesFile, "iNdIa") == "in", "india few caps")
    assert(PaleBlueDot.getCountryCode(countriesFile, "Antigua and Barbuda") == "ag", "subcordinate")

  }

}
