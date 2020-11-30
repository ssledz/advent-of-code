package aof

object Day5 extends Day {

  val day: String = "05"

  def memory: Array[Long] = lines.head.split(',').map(_.toLong)

  def solutionPartA: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(1)).output.head.toString
  }

  def solutionPartB: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(5)).output.head.toString
  }

}

object Day5App extends App {

  println("SolutionPartA: " + Day5.solutionPartA)
  println("SolutionPartB: " + Day5.solutionPartB)

}
