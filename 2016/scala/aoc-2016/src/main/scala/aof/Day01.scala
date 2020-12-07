package aof
import aof.utils._

object Day01 extends Day with App {

  val day: String = "day01.txt"

  val moves = Array((0, -1), (1, 0), (0, 1), (-1, 0))

  val movesCount = moves.size

  val turn = Map(('L', -1), ('R', 1))

  def nextMove(state: Int, action: Char): (Int, (Int, Int)) = {
    val newState = (movesCount + (state + turn(action))) % movesCount
    (newState, moves(newState))
  }

  val seq = lines.head.split(',').toList.map(_.trim).map(s => (s.charAt(0), s.substring(1).toInt))

  def solutionPartA: String = {
    val (_, pos) = seq.foldLeft((0, (0, 0))) {
      case ((state, pos), (action, dist)) =>
        val (nextState, move) = nextMove(state, action)
        (nextState, move * dist + pos)
    }
    pos.norm.toString
  }

  def solutionPartB: String = {
    val (_, _, visited) = seq.foldLeft((0, (0, 0), List.empty[(Int, Int)])) {
      case ((state, pos, visited), (action, dist)) =>
        val (nextState, move) = nextMove(state, action)
        val nextPos = move * dist + pos
        val steps = (1 to dist).map(d => move * d + pos).toList
        (nextState, nextPos, visited ::: steps)
    }

    val pos = visited.zipWithIndex.groupBy(_._1).filter(_._2.size >= 2).view.mapValues(x => x.map(_._2).min).toMap

    pos.toList.sortBy(_._2).headOption.map(_._1.norm).toString
  }

  run()
}
