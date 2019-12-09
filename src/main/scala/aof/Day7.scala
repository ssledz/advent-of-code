package aof

object Day7 extends Day {

  val day: String = "07"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def createComputer(memory: Array[Int], inA: Int, inB: Int): IntComputer =
    new IntComputer(memory, List(inA, inB), 0, false, false)

  def runAmplifier(memory: Array[Int], inA: Int, inB: Int): Int = {
    val c = createComputer(memory, inA, inB)
    c.runInterpreter
    c.out
  }


  def maxThrusterSignalForLooped(memory: () => Array[Int]): (Int, Seq[Int]) = {

    def connectAndRun(xs: Seq[Int]): Int = {

      val memories = xs.map(_ -> memory())

      def go(in: Int, acc: Int): Int = {
        if (in == 0 && acc > 0) {
          acc
        } else {
          val signal = memories.foldLeft(in) { case (inA, (inB, memory)) => runAmplifier(memory, inB, inA) }
          go(signal, if (signal == 0) acc else signal)
        }
      }

      go(0, 0)

    }

    val r = 5 to 9

    r.permutations.map { p =>
      (connectAndRun(p), p)
    }.maxBy(_._1)

  }

  def maxThrusterSignalForSerial(memory: () => Array[Int]): (Int, Seq[Int]) = {

    def connectAndRun(xs: Seq[Int]): Int =
      xs.foldLeft(0)((inA, inB) => runAmplifier(memory(), inB, inA))

    val r = 0 to 4

    r.permutations.map { p =>
      (connectAndRun(p), p)
    }.maxBy(_._1)

  }


  def solutionPartA: String = "" + maxThrusterSignalForSerial(() => memory)._1

  def solutionPartB: String = "" + maxThrusterSignalForLooped(() => memory)._1

}

object Day7App extends App {

  println("SolutionPartA: " + Day7.solutionPartA)
  println("SolutionPartB: " + Day7.solutionPartB)

}
