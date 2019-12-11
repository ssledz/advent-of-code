package aof

object Day5 extends Day {

  val day: String = "05"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def solutionPartA: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(1)).out._1.get.toString
  }

  def solutionPartB: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(5)).out._1.get.toString
  }

}

object Day5App extends App {

  println("SolutionPartA: " + Day5.solutionPartA)
  println("SolutionPartB: " + Day5.solutionPartB)

}
