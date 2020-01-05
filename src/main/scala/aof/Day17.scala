package aof

import aof.Day17.{solutionPartA, solutionPartB}

object Day17 extends Day {

  val day: String = "17"

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  val NL = 10
  val Scaffold = 35 // #
  val Intersection = 'O'.toInt
  val RobotFaces = Map(('^'.toInt, Vec(0, -1)), ('v'.toInt, Vec(0, 1)), ('<'.toInt, Vec(-1, 0)), ('>'.toInt, Vec(0, 1)))

  def buildMap(xs: List[Long]): Map[Vec, Int] = xs.map(_.toInt)
    .foldLeft((Vec.Zero, Map.empty[Vec, Int])) { case ((vec@Vec(x, y), as), a) =>
      if (a == NL) {
        (Vec(0, y + 1), as)
      } else {
        (Vec(x + 1, y), as + (vec -> a))
      }
    }._2

  def minMax(area: Map[Vec, Int]): (Vec, Vec) = area.toList
    .foldLeft((Vec.MaxValue, Vec.MinValue)) { case ((min, max), (pos, _)) =>
      (min min pos, max max pos)
    }

  def writeArea(area: Map[Vec, Int], render: Option[Int] => String): String = {
    val (min, max) = minMax(area)
    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map(x => render(area.get(Vec(x, y)))).mkString
    }.mkString("\n")
  }

  val Mask = List(Vec(0, 0), Vec(-1, 0), Vec(1, 0), Vec(0, -1), Vec(0, 1))

  val IntersectionValue = Mask.length * Scaffold

  def scaffoldInter(area: Map[Vec, Int]): Set[Vec] = {

    def opF(x: Vec): Int = Mask.map(_ + x).map(area.get).flatten.sum

    val (min, max) = minMax(area)

    val xs = for {
      x <- (min.x + 1) to (max.x - 1)
      y <- (min.y + 1) to (max.y - 1)
    } yield Vec(x, y)

    xs.map(vec => vec -> opF(vec)).filter(_._2 == IntersectionValue).map(_._1).toSet

  }

  val area = {
    val output = IntComputer(memory.toArray).extendMemory(3 * 1024)
      .runInterpreter(List.empty).output
    buildMap(output.reverse)
  }

  def solutionPartA: String = {

    val renderer: Option[Int] => String = {
      case Some(value) => value.toChar.toString
      case _ => "?"
    }

    val ints = scaffoldInter(area)

    println(writeArea(area ++ ints.map(_ -> Intersection), renderer))

    "" + ints.map { case Vec(x, y) => x * y }.sum
  }

  trait MovF

  case class Forward(x: Int) extends MovF

  case class Turn(x: String) extends MovF

  def path(area: Map[Vec, Int]): List[Vector[MovF]] = {

    def movesFrom(p: Vec, face: Int): List[(Int, Vector[MovF], Vec)] = List.empty

    def go(xs: List[(Int, Vector[MovF], Vec, Set[Vec])], acc: List[Vector[MovF]] = List.empty): List[Vector[MovF]] = xs match {
      case (_, ms, _, notVisited) :: t if notVisited.isEmpty => go(t, ms :: acc)
      case (face, ms, pos, notVisited) :: t => {
        val ys = movesFrom(pos, face)
          .filter { case (_, _, pos) => notVisited contains pos }
          .map { case (newFace, nextMs, newPos) =>
            (newFace, ms ++ nextMs, newPos, notVisited - pos)
          }
        go(ys ::: t, acc)
      }
      case Nil => acc
    }

    val faces = RobotFaces.keys.toSet
    val start: Vec = area.find { case (_, tile) => faces contains tile }.get._1
    println("start: " + start)
    go(List((area(start), Vector.empty, start, area.keys.toSet)))
  }

  def solutionPartB: String = {
    "" + path(area)
  }

  case class Vec(x: Int, y: Int) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y)

    def min(other: Vec): Vec = Vec(x min other.x, y min other.y)

    def max(other: Vec): Vec = Vec(x max other.x, y max other.y)

    def manhattanLen: Int = x + y
  }

  object Vec {
    val MaxValue = Vec(Int.MaxValue, Int.MaxValue)
    val MinValue = Vec(Int.MinValue, Int.MinValue)
    val Zero = Vec(0, 0)
  }

}

object Day17App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
