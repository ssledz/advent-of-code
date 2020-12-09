package aof

object Day02 extends Day with App {

  val day: String = "day02.txt"

  val digits = lines.map(_.split(raw"""\s+""").map(_.toInt).toList)

  def solutionPartA: String = digits.map(r => r.max - r.min).sum.toString

  def solutionPartB: String = {
    val sorted = digits.map(_.sorted(Ordering[Int].reverse))
    val dividable = sorted.flatMap { xs =>
      xs.zipWithIndex.flatMap { case (x, i) => xs.drop(i + 1).find(y => x % y == 0).map(x -> _) }
    }
    dividable.map { case (f, s) => f / s }.sum.toString
  }

  run()
}
