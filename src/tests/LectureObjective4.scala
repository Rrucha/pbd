package tests

import org.scalatest._
import pbd.PaleBlueDot

class LectureObjective4 extends FunSuite {

  val citiesFilename: String = "data/cities.csv"

  test("test 1"){
    assert(PaleBlueDot.bigCities(citiesFilename,"us","NY", 100000).sorted == List("syracuse", "yonkers", "rochester", "new york", "buffalo").sorted, "test-sub1" )
    assert(PaleBlueDot.bigCities(citiesFilename,"us","NY", 100000).sorted == List("rochester","syracuse", "yonkers", "new york", "buffalo").sorted ,"test-sub2")
  }

}
