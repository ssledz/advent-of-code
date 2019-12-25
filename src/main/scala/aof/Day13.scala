package aof

import aof.Day13.{solutionPartA, solutionPartB}

object Day13 extends Day {

  val day: String = "13"

  val Block = 2

  def memory: Array[Long] = lines.head.split(',').map(_.toLong)

  def solutionPartA: String = {
    "" + IntComputer(memory).extendMemory().runInterpreter(List.empty).output
      .sliding(3, 3)
      .map(x => x.head)
      .count(_ == Block)
  }

  def solutionPartB: String = ""

}

object Day13App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
