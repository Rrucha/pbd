package tests

import org.scalatest._
import pbd.PaleBlueDot
import pbd.PaleBlueDot.cityPopulations

class LectureObjective3 extends FunSuite {

  val citiesFilename: String = "data/cities.csv"

  test("test 1") {
    assert(PaleBlueDot.cityPopulations(citiesFilename, "zw","06") == Map("bulawayo" -> 897249, "gwanda" -> 20731, "gweru" -> 201879, "redcliffe" -> 38231), "zw")
    assert(PaleBlueDot.cityPopulations(citiesFilename, "Zw","06") == Map( ), "caps")
    assert(PaleBlueDot.cityPopulations(citiesFilename, "ad","04") == Map("la massana" -> 7211 ),"ad")
  }

}
