package aof

import aof.Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  test(s"run program ${Ex1.toList}") {
    val memory = IntComputer.extendMemory(Ex1)
    val c = new IntComputer(memory)
    assert(c.runInterpreter(List.empty).output.reverse === Ex1.toList)
  }

  test(s"run program ${Ex2.toList}") {
    val memory = IntComputer.extendMemory(Ex2)
    val c = new IntComputer(memory)
    assert(c.runInterpreter(List.empty).output.reverse === List(1219070632396864L))
  }

  test(s"run program ${Ex3.toList}") {
    val memory = IntComputer.extendMemory(Ex3)
    val c = new IntComputer(memory)
    assert(c.runInterpreter(List.empty).output.head === Ex3(1))
  }
}

object Day9 {

  val Ex1 = Array(109L, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)

  val Ex2 = Array(1102L, 34915192, 34915192, 7, 4, 7, 99, 0)

  val Ex3 = Array(104L, 1125899906842624L, 99)
}
