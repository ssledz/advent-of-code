package aof

object Day15 extends Day with App {

  val day: String = "day15.txt"

  val numbers = lines.head.split(',').map(_.toLong).toList

  def playGame(mem: Map[Long, Int], last: Long, it: Int, end: Int): Long =
    if (it - 1 == end) {
      last
    } else {
      val newLast = if (mem.contains(last)) it - 1 - mem(last) else 0
      playGame(mem.updated(last, it - 1), newLast, it + 1, end)
    }

  def solutionPartA: String = playGame((0L :: numbers).init.zipWithIndex.drop(1).toMap, numbers.last, numbers.length + 1, 2020).toString

  def solutionPartB: String = playGame((0L :: numbers).init.zipWithIndex.drop(1).toMap, numbers.last, numbers.length + 1, 30000000).toString

  run()
}
