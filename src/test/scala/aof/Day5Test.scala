package aof

import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  test("run program (1002, 4, 3, 4, 33)") {
    val memory = Array(1002L, 4, 3, 4, 33)
    val c = new IntComputer(memory)
    c.runInterpreter(List(1))
    assert(memory.toList === List(1002L, 4, 3, 4, 99))
  }

  test("run program (1101, 100, -1, 4, 0)") {
    val memory = Array(1101L, 100, -1, 4, 0)
    val c = new IntComputer(memory)
    c.runInterpreter(List(1))
    assert(memory.toList === List(1101L, 100, -1, 4, 99))
  }

  test("run program (3, 0, 4, 0, 99)") {
    def memory = Array(3L, 0, 4, 0, 99)

    val c = new IntComputer(memory)
    c.runInterpreter(List(11))
    assert(c.m.toList === List(11L, 0, 4, 0, 99))

    val c2 = new IntComputer(memory).runInterpreter(List.empty)
    assert(c2.waitingInput)
    assert(c2.output == List.empty)
    assert(c2.runInterpreter(List(-1)).output == List(-1))

  }

}
