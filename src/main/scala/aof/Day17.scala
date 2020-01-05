package aof

import aof.Day17.{solutionPartA, solutionPartB}

object Day17 extends Day {

  val day: String = "17"

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  val NL = 10

  def buildMap(xs: List[Long]): Map[Vec, Int] = xs.map(_.toInt).foldLeft((Vec.Zero, Map.empty[Vec, Int])) { case ((vec@Vec(x, y), as), a) =>
    if (a == NL) {
      (Vec(0, y + 1), as)
    } else {
      (Vec(x + 1, y), as + (vec -> a))
    }
  }._2

  def writeArea(area: Map[Vec, Int], render: Option[Int] => String): String = {

    val (min, max) = area.toList
      .foldLeft((Vec.MaxValue, Vec.MinValue)) { case ((min, max), (pos, _)) =>
        (min min pos, max max pos)
      }

    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map(x => render(area.get(Vec(x, y)))).mkString
    }.mkString("\n")
  }

  def solutionPartA: String = {

    val output = IntComputer(memory.toArray).extendMemory(3 * 1024)
      .runInterpreter(List.empty).output

    val area = buildMap(output.reverse)

    val renderer: Option[Int] => String = {
      case Some(value) => value.toChar.toString
      case _ => "?"
    }

    "\n" + writeArea(area, renderer)
  }

  def solutionPartB: String = ""

  case class Vec(x: Int, y: Int) {
    def min(other: Vec): Vec = Vec(x min other.x, y min other.y)

    def max(other: Vec): Vec = Vec(x max other.x, y max other.y)
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
