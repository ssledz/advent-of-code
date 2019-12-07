package aof

object Day2 extends Day with App {
  val day = "02"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def runIntInterpreter(m: Array[Int], pc: Int): Array[Int] = {

    def op(f: (Int, Int) => Int): Unit = {
      val posA = m(pc + 1)
      val posB = m(pc + 2)
      val dest = m(pc + 3)
      m(dest) = f(m(posA), m(posB))
    }

    val opCode = m(pc)

    if (opCode == 1) {
      op(_ + _)
      runIntInterpreter(m, pc + 4)
    } else if (opCode == 2) {
      op(_ * _)
      runIntInterpreter(m, pc + 4)
    } else if (opCode == 99) {
      m
    } else {
      throw new IllegalStateException(s"Illegal op code=$opCode, pc=$pc")
    }

  }

  runIntInterpreter(Array(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), 0).toList === List(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
  runIntInterpreter(Array(1, 0, 0, 0, 99), 0).toList === List(2, 0, 0, 0, 99)
  runIntInterpreter(Array(2, 3, 0, 3, 99), 0).toList === List(2, 3, 0, 6, 99)
  runIntInterpreter(Array(2, 4, 4, 5, 99, 0), 0).toList === List(2, 4, 4, 5, 99, 9801)
  runIntInterpreter(Array(1, 1, 1, 4, 99, 5, 6, 0, 99), 0).toList === List(30, 1, 1, 4, 2, 5, 6, 0, 99)

  def run(noun: Int, verb: Int): Int = {

    val m = memory

    m(1) = noun
    m(2) = verb

    runIntInterpreter(m, 0).toList.head

  }

  println("part1: " + run(12, 2))

  val r = 0 to 99

  val zr = for {
    a <- r
    b <- r
  } yield (a, b)


  val Some((noun, verb)) = zr.find { case (noun, verb) =>
    run(noun, verb) == 19690720
  }


  println("part2: " + (100 * noun + verb))

}
