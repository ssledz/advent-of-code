package aof

object Day01 extends Day with App {

  val day: String = "day01.txt"

  val digits = lines.head.map(_ - '0').toList

  def calc(pairs: List[(Int, Int)]): Int = pairs.filter { case (f, s) => f == s }.map(_._1).sum

  def solutionPartA: String = calc(digits.zip(digits.drop(1) ::: List(digits.head))).toString

  def solutionPartB: String = digits.splitAt(digits.length / 2) match {
    case (l1, l2) => calc(digits.zip(l2 ::: l1)).toString
  }

  run()
}
