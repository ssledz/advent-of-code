package aof

import aof.Day18.{solutionPartA, solutionPartB}

object Day18 extends Day {

  val day: String = "18"

  def buildMap(xs: List[Char]): Map[Vec, Char] =
    xs.foldLeft((Vec.Zero, Map.empty[Vec, Char])) { case ((vec@Vec(x, y), as), a) =>
      if (a == '\n') {
        (Vec(0, y + 1), as)
      } else {
        (Vec(x + 1, y), as + (vec -> a))
      }
    }._2

  def writeArea(area: Map[Vec, Char]): String = {
    def minMax(area: Map[Vec, Char]): (Vec, Vec) = area.toList
      .foldLeft((Vec.MaxValue, Vec.MinValue)) { case ((min, max), (pos, _)) =>
        (min min pos, max max pos)
      }

    val (min, max) = minMax(area)
    println(min + " " + max)
    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map(x => area(Vec(x, y))).mkString
    }.mkString("\n")
  }

  def buildGraph(area : Map[Vec, Char]) : Graph = {
    ???
  }

  def solutionPartA: String = {

    val area = buildMap(lines.flatMap(_.toList :+ '\n'))
    println(writeArea(area))

    ""
  }

  def solutionPartB: String = ""

}

object Day18App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
