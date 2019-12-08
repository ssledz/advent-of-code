package aof

import aof.Day7.maxThrusterSignal
import aof.Day7Test._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  test("maxThrusterSignal") {
    assert(maxThrusterSignal(() => Ex1) === (43210, Seq(4, 3, 2, 1, 0)))
    assert(maxThrusterSignal(() => Ex2) === (54321, Seq(0, 1, 2, 3, 4)))
    assert(maxThrusterSignal(() => Ex3) === (65210, Seq(1, 0, 4, 3, 2)))
  }

}

object Day7Test {

  def Ex1 = Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0)
  def Ex2 = Array(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0)
  def Ex3 = Array(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)

}