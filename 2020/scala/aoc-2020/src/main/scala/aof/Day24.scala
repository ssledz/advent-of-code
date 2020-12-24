package aof
import aof.utils._

import scala.annotation.tailrec
object Day24 extends Day with App {

  val day: String = "day24.txt"

  val dirs = Map(
    ("e", (2, 0)),
    ("se", (1, 1)),
    ("sw", (-1, 1)),
    ("w", (-2, 0)),
    ("nw", (-1, -1)),
    ("ne", (1, -1))
  )

  val blackTiles: List[(Int, Int)] = {
    @tailrec
    def traverse(xs: List[Char], pos: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] =
      xs match {
        case h :: t if h == 'e' || h == 'w' => traverse(t, dirs(h.toString) + pos, pos :: acc)
        case a :: b :: t                    => traverse(t, dirs(List(a, b).mkString) + pos, pos :: acc)
        case Nil                            => pos :: acc
      }
    val pos = lines.map(l => traverse(l.toList, (0, 0), List.empty).head)
    val xs = pos.groupBy(identity).view.mapValues(_.size).toMap
    xs.filter { case (_, cnt) => cnt % 2 == 1 }.keys.toList
  }

  def solutionPartA: String = blackTiles.size.toString

  def solutionPartB: String =
    ""

  run()
}
