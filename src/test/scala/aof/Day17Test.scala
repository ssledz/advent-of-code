package aof

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  test("asciiEncode") {
    assert(Day17.asciiEncode(Seq("A", "B", "C", "B", "A", "C")) == Seq(65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67, 10))
    assert(Day17.asciiEncode(Seq("R", "8", "R", "8")) == Seq(82, 44, 56, 44, 82, 44, 56, 10))
    assert(Day17.asciiEncode(Seq("R", "4", "R", "4", "R", "8")) == Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56, 10))
    assert(Day17.asciiEncode(Seq("L", "6", "L", "2")) == Seq(76, 44, 54, 44, 76, 44, 50, 10))
    assert(Day17.asciiEncode(Seq("L")) == Seq(76, 10))
    assert(Day17.asciiEncode(Seq("L", "84")) == Seq(76, 44, 56, 52, 10))
  }

}
