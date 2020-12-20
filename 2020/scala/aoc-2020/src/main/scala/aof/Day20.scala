package aof

object Day20 extends Day with App {

  type Image = Vector[Vector[Char]]

  val day: String = "day20.txt"

  def parse: List[Tile] = {
    def go(xs: List[String], acc: List[Tile]): List[Tile] = xs match {
      case h :: t if h.isBlank => go(t, acc)
      case h :: _ if h.startsWith("Tile") =>
        val (rest, tile) = Tile.from(xs)
        go(rest, tile :: acc)
      case Nil => acc
    }
    go(lines, List.empty)
  }

  val tiles = parse

  def solutionPartA: String = {
    println(tiles.map(_.toString).mkString("\n"))
    ""
  }

  def solutionPartB: String = ""

  run()

  case class Tile(id: Int, img: Image)
  object Tile {
    val r = """Tile (\d+):""".r
    def from(xs: List[String]): (List[String], Tile) = {
      val id = xs.head.trim match {
        case r(num) => num.toInt
      }
      val (ys, rest) = xs.tail.span(x => !x.isBlank)
      (rest, Tile(id, ys.map(_.toVector).toVector))
    }
  }
}
