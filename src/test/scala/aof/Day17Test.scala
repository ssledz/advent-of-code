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

  test("encodeFunctions") {
    val xs = Day17.asciiEncode(Day17Test.Ex1)
    val ys = Day17.encodeFunctions(xs)
    assert(ys.map(_._1) contains Seq(82, 44, 56, 44, 82, 44, 56))
    assert(ys.map(_._2) contains Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56))
    assert(ys.map(_._3) contains Seq(76, 44, 54, 44, 76, 44, 50))
  }

  test("encodeRoutines") {
    val xs = Day17.asciiEncode(Day17Test.Ex1)
    val aF = Seq(82, 44, 56, 44, 82, 44, 56)
    val bF = Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56)
    val cF = Seq(76, 44, 54, 44, 76, 44, 50)
    val ys = Day17.encodeRoutines(aF, bF, cF, xs)
    assert(ys == Seq(65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67))
  }
}

object Day17Test {

  val Ex1 = Seq("R", "8", "R", "8", "R", "4", "R", "4", "R", "8", "L", "6", "L", "2", "R", "4", "R", "4", "R", "8", "R", "8", "R", "8", "L", "6", "L", "2")

}
