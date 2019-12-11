package aof

object Day9 extends Day {

  val day: String = "09"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def solutionPartA: String = {
    val c = new IntComputer(memory)
    "" + c.runInterpreter(List(1)).output.mkString(",")
  }

  def solutionPartB: String = ""

}

object Day9App extends App {
  // your answer is too low, you guessed 1102
  println("SolutionPartA: " + Day9.solutionPartA)
  println("SolutionPartB: " + Day9.solutionPartB)
}