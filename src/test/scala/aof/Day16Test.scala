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

  test("fft") {
    assert(asString(Day16.fft(in("12345678"))) == "48226158")
    assert(asString(Day16.fft(in("48226158"))) == "34040438")
    assert(asString(Day16.fft(in("34040438"))) == "03415518")
    assert(asString(Day16.fft(in("03415518"))) == "01029498")
  }

  test("ffts") {
    assert(asString(Day16.ffts(in("12345678"), 4)) == "01029498")
    assert(asString(Day16.ffts(in("80871224585914546619083218645595"), 100).take(8)) == "24176176")
    assert(asString(Day16.ffts(in("19617804207202209144916044189917"), 100).take(8)) == "73745418")
    assert(asString(Day16.ffts(in("69317163492948606335995924319873"), 100).take(8)) == "52432133")
  }

  test("decode") {
    assert(asString(Day16.decode(in("03036732577212944063491565474664"))) == "84462026")
    assert(asString(Day16.decode(in("02935109699940807407585447034323"))) == "78725270")
    assert(asString(Day16.decode(in("03081770884921959731165446850517"))) == "53553731")
  }

  def in(s: String) = s.toList.map(_ - '0')

  def asString(xs: List[Int]) = xs.map(_.toString).mkString

}
