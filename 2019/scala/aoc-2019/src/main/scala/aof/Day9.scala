package aof

object Day9 extends Day {

  val day: String = "09"

  def memory: Array[Long] = lines.head.split(',').map(_.toLong)

  def solutionPartA: String = {
    val c = new IntComputer(memory).extendMemory()
    "" + c.runInterpreter(List(1)).output.mkString(",")
  }

  def solutionPartB: String = {
    val c = new IntComputer(memory).extendMemory()
    "" + c.runInterpreter(List(2)).output.mkString(",")
  }

}

object Day9App extends App {
  println("SolutionPartA: " + Day9.solutionPartA)
  println("SolutionPartB: " + Day9.solutionPartB)
}