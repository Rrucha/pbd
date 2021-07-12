package tests

import org.scalatest._
import pbd.PaleBlueDot


class LectureObjective5 extends FunSuite {

  val EPSILON: Double = 0.001

  def compareDoubles(d1: Double, d2: Double): Boolean = {
    Math.abs(d1 - d2) < EPSILON
  }


  test("test 1") {

    assert(compareDoubles(PaleBlueDot.greaterCircleDistance(List(20,20),List(10,10)), 1544.757600000 ),"test-sub1")
    assert(compareDoubles(PaleBlueDot.greaterCircleDistance(List(-20,20),List(10,10)), 3510.84886000 ),"test-sub2")
    assert(compareDoubles(PaleBlueDot.greaterCircleDistance(List(20,-20),List(10,10)), 3401.5201163 ), "test-sub3")
    assert(compareDoubles(PaleBlueDot.greaterCircleDistance(List(20,20),List(-10,10)), 3510.848862222 ), "test-sub4")
    assert(compareDoubles(PaleBlueDot.greaterCircleDistance(List(20,20),List(10,-10)), 3401.5201163 ),"test-sub5")
  }

}
