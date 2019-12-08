package aof

object Day7 extends Day {

  val day: String = "07"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def runAmplifier(memory: Array[Int], inA: Int, inB: Int): Int = {
    val c = new IntComputer(memory, List(inA, inB), 0, false, false)
    c.runInterpreter
    c.out
  }


  def maxThrusterSignal(memory: () => Array[Int]): (Int, Seq[Int]) = {

    def connectAndRun(xs: Seq[Int]): Int =
      xs.foldLeft(0)((inA, inB) => runAmplifier(memory(), inB, inA))

    val r = 0 to 4

    r.permutations.map { p =>
      (connectAndRun(p), p)
    }.maxBy(_._1)

  }


  def solutionPartA: String = "" + maxThrusterSignal(() => memory)._1

  def solutionPartB: String = ""

}

object Day7App extends App {

  println("SolutionPartA: " + Day7.solutionPartA)
  println("SolutionPartB: " + Day7.solutionPartB)

}
