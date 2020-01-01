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

  def explore: (List[Vector[Int]], Map[Vec, Int]) = {

    def movesFrom(moves: Vector[Int], pos: Vec): List[(Vector[Int], Vec)] =
      allMoves.map { m => (moves :+ m, id2move(m) + pos) }

    def go(moves: List[(Vector[Int], Vec)], visited: Set[Vec] = Set(Vec.Zero), acc: List[Vector[Int]] = List.empty,
           area: Map[Vec, Int] = Map(Vec.Zero -> Status.Moved)): (List[Vector[Int]], Map[Vec, Int]) = moves match {

      case (h, pos) :: t if !(visited contains pos) =>
        val c = IntComputer(memory.toArray).extendMemory().runInterpreter(h)
        val status = c.output.head.toInt
        if (status == Status.OxygenStation) {
          (h :: acc, area + (pos -> status))
        } else if (status == Status.HitWall) {
          go(t, visited + pos, acc, area + (pos -> status))
        } else {
          val newMoves = movesFrom(h, pos).filterNot { case (_, pos) => visited contains pos }
          go(newMoves ::: moves, visited + pos, acc, area + (pos -> status))
        }
      case _ :: t => go(t, visited, acc, area)
      case Nil => acc -> area

    }

    go(movesFrom(Vector.empty, Vec.Zero))
  }

  case class Vec(x: Int, y: Int) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y)

    def min(other: Vec): Vec = Vec(x min other.x, y min other.y)

    def max(other: Vec): Vec = Vec(x max other.x, y max other.y)
  }

  object Vec {
    val Zero = Vec(0, 0)
  }

  def writeArea(area: Map[Vec, Int]): String = {

    def statusAsStr(s: Int): String = if (s == Status.Moved) "."
    else if (s == Status.HitWall) "#" else if (s == -1) "S" else "O"

    val (min, max) = area.toList
      .foldLeft((Vec(Int.MaxValue, Int.MaxValue), Vec(Int.MinValue, Int.MinValue))) { case ((min, max), (pos, _)) =>
        (min min pos, max max pos)
      }

    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map { x =>
        val status = area.get(Vec(x, y))
        status.map(statusAsStr).getOrElse("#")
      }.mkString
    }.mkString("\n")
  }


  def solutionPartA: String = "" + explore._1.map(_.length).sorted.headOption

  def spreadOxygen: (Int, Set[Vec]) = {

    val (_, area) = explore

    val Some((oxygenStationLoc, _)) = area.find { case (_, status) => status == Status.OxygenStation }

    val moves = id2move.values.toList

    def neighbours(v: Vec): List[Vec] = moves.map(_ + v)

    def canSpread(p: Vec): Boolean = area.get(p).exists(_ != Status.HitWall)

    def go(sources: List[Vec], covered: Set[Vec] = Set.empty, it: Int = 0): (Int, Set[Vec]) = {

      val newSources = sources.flatMap(neighbours)
        .filterNot(covered.contains)
        .filter(canSpread)

      if (newSources.isEmpty) {
        it -> covered
      } else {
        go(newSources, covered ++ newSources ++ sources, it + 1)
      }

    }


    go(List(oxygenStationLoc))
  }

  def solutionPartB: String = {

    val (_, area) = explore

    val (minutes, covered) = spreadOxygen

    val xs = covered.toList.map(_ -> Status.OxygenStation)

    println(writeArea(area ++ xs))

    "" + minutes
  }

}

object Day15App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
