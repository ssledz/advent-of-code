package aof

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  test("asciiEncode") {
    assert(Day17.asciiEncode(Seq("A", "B", "C", "B", "A", "C")) == Seq(65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67))
    assert(Day17.asciiEncode(Seq("R", "8", "R", "8")) == Seq(82, 44, 56, 44, 82, 44, 56))
    assert(Day17.asciiEncode(Seq("R", "4", "R", "4", "R", "8")) == Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56))
    assert(Day17.asciiEncode(Seq("L", "6", "L", "2")) == Seq(76, 44, 54, 44, 76, 44, 50))
    assert(Day17.asciiEncode(Seq("L")) == Seq(76))
    assert(Day17.asciiEncode(Seq("L", "84")) == Seq(76, 44, 56, 52))
  }

  test("encodeFunction") {

    println(Day17.encodeFunction(Day17Test.Ex1, "A"))


  }

//  test("encodeFunctions") {
//    val ys = Day17.encodeFunctions(Day17Test.Ex1)
//    assert(ys.map(_._1).map(Day17.encodeFunctions) contains Seq(82, 44, 56, 44, 82, 44, 56))
//    assert(ys.map(_._2).map(Day17.encodeFunctions) contains Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56))
//    assert(ys.map(_._3).map(Day17.encodeFunctions) contains Seq(76, 44, 54, 44, 76, 44, 50))
//  }
//
//  test("encodeRoutines") {
//    val aF = Seq("R", "8", "R", "8")
//    val bF = Seq("R", "4", "R", "4", "R", "8")
//    val cF = Seq("L", "6", "L", "2")
//    val ys = Day17.encodeRoutines(aF, bF, cF, Day17Test.Ex1)
//    assert(ys == Seq("A", "B", "C", "B", "A", "C"))
//  }
}

object Day17Test {

  val Ex1 = List("R", "8", "R", "8", "R", "4", "R", "4", "R", "8", "L", "6", "L", "2", "R", "4", "R", "4", "R", "8", "R", "8", "R", "8", "L", "6", "L", "2")

}
