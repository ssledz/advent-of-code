package aof

object Day01 extends Day with App {

  val day: String = "day01.txt"

  val xs = lines.map(_.toInt)

  def solutionPartA: String = {
    val ys = for {
      x <- xs
      y <- xs if x + y == 2020
    } yield x * y
    ys.head.toString
  }

  def solutionPartB: String = {
    val ys = for {
      x <- xs
      y <- xs
      z <- xs if x + y + z == 2020
    } yield x * y * z
    ys.head.toString
  }

  run()
}
