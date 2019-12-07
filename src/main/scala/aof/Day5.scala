package aof

object Day5 extends Day {

  val day: String = "05"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def solutionPartA: String = {
    val c = new IntComputer(memory, 1)
    c.runInterpreter
    c.out.toString
  }

  def solutionPartB: String = {
    ""
  }

}

class IntComputer(m: Array[Int], input: Int, output: Int = 0) {

  def out: Int = m(output)

  def in: Int = m(input)

  def runInterpreter: Array[Int] = run(0)

  private def run(pc: Int): Array[Int] = {

    def arthmeticOp(f: (Int, Int) => Int): Unit = {
      val posA = m(pc + 1)
      val posB = m(pc + 2)
      val dest = m(pc + 3)
      m(dest) = f(m(posA), m(posB))
    }

    val opCode = m(pc)

    opCode match {
      case 1 => arthmeticOp(_ + _)
        run(pc + 4)
      case 2 => arthmeticOp(_ * _)
        run(pc + 4)
      case 3 =>
        m(m(pc + 1)) = in
        run(2)
      case 4 =>
        m(output) = m(m(pc + 1))
        run(2)
      case 99 => m
      case _ => throw new IllegalStateException(s"Illegal op code=$opCode, pc=$pc")
    }
  }

}

object Day5App extends App {

  println("SolutionPartA: " + Day5.solutionPartA)
  println("SolutionPartB: " + Day5.solutionPartB)

}
