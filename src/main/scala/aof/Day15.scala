package aof

import aof.Day15.{solutionPartA, solutionPartB}

object Day15 extends Day {

  val day: String = "15"

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  val allMoves = (1 to 4).toList

  val id2move = Map((1, Vec(0, -1)), (2, Vec(0, 1)), (3, Vec(-1, 0)), (4, Vec(1, 0)))

  object Status {
    val HitWall = 0
    val Moved = 1
    val OxygenStation = 2
  }

  object Tile {
    val Wall = "#"
    val Empty = "."
    val Droid = "D"
    val OxygenStation = "O"
    val Start = "S"
  }

  def explore: List[Vector[Int]] = {

    def movesFrom(moves: Vector[Int], pos: Vec): List[(Vector[Int], Vec)] =
      allMoves.map { m => (moves :+ m, id2move(m) + pos) }

    def go(moves: List[(Vector[Int], Vec)], visited: Set[Vec] = Set(Vec.Zero),
           acc: List[Vector[Int]] = List.empty): List[Vector[Int]] = moves match {

      case (h, pos) :: t if !(visited contains pos) =>
        val c = IntComputer(memory.toArray).extendMemory().runInterpreter(h)
        val status = c.output.head
        if (status == Status.OxygenStation) {
          h :: acc
        } else if (status == Status.HitWall) {
          go(t, visited + pos, acc)
        } else {
          val newMoves = movesFrom(h, pos).filterNot { case (_, pos) => visited contains pos }
          go(newMoves ::: moves, visited + pos, acc)
        }
      case _ :: t => go(t, visited, acc)
      case Nil => acc

    }

    go(movesFrom(Vector.empty, Vec.Zero))
  }

  case class Vec(x: Int, y: Int) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y)
  }

  object Vec {
    val Zero = Vec(0, 0)
  }

  def solutionPartA: String = "" + explore.map(_.length).sorted.headOption

  def solutionPartB: String = ""

}

object Day15App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
