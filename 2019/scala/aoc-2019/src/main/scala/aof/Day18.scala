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
    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map(x => area(Vec(x, y))).mkString
    }.mkString("\n")
  }

  val Passage = '.'

  val Wall = '#'

  val Entrance = '@'

  def canPassThrough(c: Char): Boolean = c == Passage || c == Entrance || isKey(c)

  def isDoor(c: Char): Boolean = c.isLetter && c.isUpper

  def isKey(c: Char): Boolean = c.isLetter && c.isLower

  val Moves = Vector(Vec(-1, 0), Vec(1, 0), Vec(0, -1), Vec(0, 1))

  case class MapLabel(vec: Vec, c: Char) extends Graph.Label

  def buildGraph(area: Map[Vec, Char]): (Map[Vec, Int], Graph) = {
    val openPassage = area.filter { case (_, c) => canPassThrough(c) }

    val vs = openPassage.toSeq.zipWithIndex

    val vec2Vert = vs.map { case ((vec, c), v) => vec -> v }.toMap

    val vLabel: Map[Int, MapLabel] = vs.map { case ((vec, c), v) => v -> MapLabel(vec, c) }.toMap

    val edges = vs.flatMap { case ((vec, c), v) =>
      Moves.map(_ + vec).filter(openPassage.contains).map(e => v -> Graph.Edge(vec2Vert(e)))
    }

    vec2Vert -> Graph(vs.size, edges: _*).withVLabel(vLabel)
  }

  def swap[A, B](m: Map[A, B]): Map[B, A] = m.toSeq.map { case (a, b) => b -> a }.toMap

  def solutionPartA: String = {

    val area = buildMap(lines.flatMap(_.toList :+ '\n'))

    val (entrance, _) = area.find { case (_, c) => c == '@' }.get
    val doors = swap(area.filter { case (_, c) => isDoor(c) })
    val keys = swap(area.filter { case (_, c) => isKey(c) })

    val (vec2Vert, g) = buildGraph(area)

    val vert2Vec: Map[Int, Vec] = swap(vec2Vert)

    val entranceV = vec2Vert(entrance)

    println("entrance: " + entrance + s" ($entranceV)")
    println("doors: " + doors)
    println("keys: " + keys)

    val parents = Graph.bfs(g, entranceV)
    val gV = keys('g')
    val pathG = Graph.findPath(entranceV, vec2Vert(gV), parents).map(vert2Vec).map(_ -> 'x')
    println(pathG)

    println(writeArea(area ++ pathG + (keys('g') -> 'g')))

    ""
  }

  def solutionPartB: String = ""

}

object Day18App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
