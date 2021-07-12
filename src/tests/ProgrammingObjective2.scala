package tests

import org.scalatest._
import pbd.PaleBlueDot
import pbd.PaleBlueDot.countryPopulation

class ProgrammingObjective2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1"){
    assert(PaleBlueDot.countryPopulation( countriesFile, citiesFilename, "iNdia") == 259227307)
  }

}
