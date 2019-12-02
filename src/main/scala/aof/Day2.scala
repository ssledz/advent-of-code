package aof

object Day2 extends Day with App {
  val day = "02"

  val memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def runProgram(m: Array[Int], pc: Int): Array[Int] = {

    def op(f: (Int, Int) => Int): Unit = {
      val posA = m(pc + 1)
      val posB = m(pc + 2)
      val dest = m(pc + 3)
      m(dest) = f(m(posA), m(posB))
    }

    val opCode = m(pc)

    if (opCode == 1) {
      op(_ + _)
      runProgram(m, pc + 4)
    } else if (opCode == 2) {
      op(_ * _)
      runProgram(m, pc + 4)
    } else if (opCode == 99) {
      m
    } else {
      throw new IllegalStateException(s"Illegal op code=$opCode, pc=$pc")
    }

  }

  runProgram(Array(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), 0).toList === List(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
  runProgram(Array(1, 0, 0, 0, 99), 0).toList === List(2, 0, 0, 0, 99)
  runProgram(Array(2, 3, 0, 3, 99), 0).toList === List(2, 3, 0, 6, 99)
  runProgram(Array(2, 4, 4, 5, 99, 0), 0).toList === List(2, 4, 4, 5, 99, 9801)
  runProgram(Array(1, 1, 1, 4, 99, 5, 6, 0, 99), 0).toList === List(30, 1, 1, 4, 2, 5, 6, 0, 99)

  memory(1) = 12
  memory(2) = 2

  println(runProgram(memory, 0).toList)
}
