package aof
import aof.utils._

import scala.annotation.tailrec
object Day24 extends Day with App {

  type Point = (Int, Int)

  val day: String = "day24.txt"

  val dirs = Map(
    ("e", (2, 0)),
    ("se", (1, 1)),
    ("sw", (-1, 1)),
    ("w", (-2, 0)),
    ("nw", (-1, -1)),
    ("ne", (1, -1))
  )

  val blackTiles: List[Point] = {
    @tailrec
    def traverse(xs: List[Char], pos: Point, acc: List[Point]): List[Point] =
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

  def adjacentTiles(p: Point): List[Point] = dirs.values.toList.map(_ + p)

  def solutionPartB: String = {
    val day0 = blackTiles.toSet
    def go(blackTiles: Set[Point]): Set[Point] = {
      val space = blackTiles.flatMap(adjacentTiles) ++ blackTiles
      space.flatMap { tile =>
        val adj = adjacentTiles(tile)
        val cnt = adj.count(blackTiles.contains)
        if (blackTiles.contains(tile)) {
          if (cnt == 0 || cnt > 2) List.empty else List(tile)
        } else {
          if (cnt == 2) List(tile) else List.empty
        }
      }
    }

    def goN(n: Int): Set[Point] =
      (1 to n)
        .foldLeft(day0) {
          case (black, _) => go(black)
        }

    goN(100).size.toString
  }

  run()
}
