package tests

import org.scalatest._
import pbd.PaleBlueDot


class LectureObjective1 extends FunSuite {

  val EPSILON: Double = 0.0000001

  def compareDoubles(d1: Double, d2: Double): Boolean = {
    Math.abs(d1 - d2) < EPSILON
  }

  test("Doubles are checked for size in each category") {

    assert( compareDoubles(PaleBlueDot.degreesToRadians(70), 1.2217304764))
    assert( compareDoubles(PaleBlueDot.degreesToRadians(10), 0.17453292519))
    assert( compareDoubles(PaleBlueDot.degreesToRadians(50), 0.87266462599))

  }
}