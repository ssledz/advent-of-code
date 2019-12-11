package aof

import aof.Day9.Ex9
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  test(s"run program ${Ex9.toList}") {
    val memory = IntComputer.extendMemory(Ex9)
    val c = new IntComputer(memory)
    assert(c.runInterpreter(List.empty).output.reverse === Ex9.toList)
  }

}

object Day9 {

  val Ex9 = Array(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)

}
