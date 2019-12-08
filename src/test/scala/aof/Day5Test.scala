package aof

import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  test("run program (1002, 4, 3, 4, 33)") {
    val memory = Array(1002, 4, 3, 4, 33)
    val c = new IntComputer(memory, List(1))
    c.runInterpreter
    assert(memory.toList === List(1002, 4, 3, 4, 99))
  }

  test("run program (1101, 100, -1, 4, 0)") {
    val memory = Array(1101, 100, -1, 4, 0)
    val c = new IntComputer(memory, List(1))
    c.runInterpreter
    assert(memory.toList === List(1101, 100, -1, 4, 99))
  }

  test("run program (3, 0, 4, 0, 99)") {
    val memory = Array(3, 0, 4, 0, 99)
    val c = new IntComputer(memory, List(11))
    c.runInterpreter
    assert(memory.toList === List(11, 0, 4, 0, 99))
  }

}
