package aof

import aof.utils._

object Day03 extends Day with App {

  val day: String = "day03.txt"

  val moves = Map(('>', (1, 0)), ('<', (-1, 0)), ('^', (0, -1)), ('v', (0, 1)))

  val dirs = lines.flatMap(_.toCharArray.toList)

  def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)

  def travers(dirs: List[Char], zero: (Int, Int) = (0, 0)): List[(Int, Int)] = {
    def go(dirs: List[Char], curr: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = dirs match {
      case h :: t =>
        val move = moves(h)
        go(t, add(curr, move), curr :: acc)
      case Nil => curr :: acc
    }

    go(dirs, zero, List.empty)
  }

  def frequency(dirs: List[Char]) =
    travers(dirs).map(_ -> 1).groupBy(_._1).view.mapValues(_.map(_._2).sum)

  def solutionPartA: String = {
    val freq = frequency(dirs)
    freq.size.toString
  }

  def solutionPartB: String = {
    val (dirs1, dirs2) = spanEven(dirs)
    val freq1 = frequency(dirs1).toMap
    val freq2 = frequency(dirs2).toMap
    implicit val intSemi: Semigroup[Int] = (x, y) => x + y
    val freq = mergeMaps(freq1, freq2)
    freq.size.toString
  }

  run()
}
