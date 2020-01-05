package aof

import aof.Day17.{solutionPartA, solutionPartB}

object Day17 extends Day {

  val day: String = "17"

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  val NL = 10
  val Scaffold = 35 // #
  val Intersection = 'O'.toInt

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

  def solutionPartA: String = {

    val output = IntComputer(memory.toArray).extendMemory(3 * 1024)
      .runInterpreter(List.empty).output

    val area = buildMap(output.reverse)

    val renderer: Option[Int] => String = {
      case Some(value) => value.toChar.toString
      case _ => "?"
    }

    val ints = scaffoldInter(area)

    println(writeArea(area ++ ints.map(_ -> Intersection), renderer))

    "" + ints.map { case Vec(x, y) => x * y }.sum
  }

  def solutionPartB: String = ""

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
