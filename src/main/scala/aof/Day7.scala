package aof

import scala.annotation.tailrec

object Day7 extends Day {

  val day: String = "07"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def runAmplifier(memory: Array[Int], inA: Int, inB: Int): Int = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(inA, inB))
    c.out
  }


  def maxThrusterSignalForLooped(memory: () => Array[Int]): (Int, Seq[Int]) = {

    def connectAndRun(xs: Seq[Int]): Int = {

      val computers = xs.map(seq => new IntComputer(memory()).runInterpreter(List(seq)))

      @tailrec
      def go(in: Int, toProcess: List[IntComputer], computers: List[IntComputer], thrusters: Int): Int = toProcess match {
        case c :: t => {
          if (c.halt) {
            thrusters
          } else {
            val cc = c.runInterpreter(List(in))
            go(cc.out, t, cc :: computers, thrusters)
          }
        }
        case Nil => go(in, computers.reverse, List.empty, in)
      }

      go(0, computers.toList, List.empty, 0)

    }

    val r = 5 to 9

    r.permutations.map { p =>
      (connectAndRun(p), p)
    }.maxBy(_._1)

  }

  def maxThrusterSignalForSerial(memory: () => Array[Int]): (Int, Seq[Int]) = {

    def connectAndRun(xs: Seq[Int]): Int =
      xs.foldLeft(0)((inB, inA) => runAmplifier(memory(), inA, inB))

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
