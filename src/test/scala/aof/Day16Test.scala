package aof

import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  test("repeat") {
    val xs = List(9, 8, 7, 6, 5).zip(Day16.repeat(1, List(1, 2, 3)))

    assert(xs == List((9, 1), (8, 2), (7, 3), (6, 1), (5, 2)))

    val pattern = List(0, 1, 0, -1)

    val ys = Day16.repeat(3, pattern).take(12).toList

    assert(ys == List(0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1))

    val zs = Day16.repeat(2, pattern).drop(1).take(15).toList

    assert(zs == List(0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1))
  }

}
