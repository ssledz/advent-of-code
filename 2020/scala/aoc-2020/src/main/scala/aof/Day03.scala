package aof

import scala.annotation.tailrec

object Day03 extends Day with App {

  val day: String = "day03.txt"

  val arr = lines.map(_.toCharArray).toArray
  val maxI = arr.length
  val maxJ = arr(0).length

  def jj(j: Int): Int = j % maxJ

  @tailrec
  def encountered(i: Int, j: Int, cnt: Int, di: Int, dj: Int): Int = {
    val iii = i + di
    if (iii >= maxI) {
      cnt
    } else {
      val jjj = jj(j + dj)
      val dCnt = if (arr(iii)(jjj) == '#') 1 else 0
      encountered(iii, jjj, cnt + dCnt, di, dj)
    }
  }

  def solutionPartA: String = encountered(0, 0, 0, 1, 3).toString

  def solutionPartB: String = {

    val xs = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map { case (dj, di) => encountered(0, 0, 0, di, dj) }

    val cnt = xs.foldLeft(1L)(_ * _)

    cnt.toString
  }

  run()
}
