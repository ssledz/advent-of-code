package aof

object Day20 extends Day with App {

  type Image = Vector[Vector[Char]]

  val day: String = "day20.txt"
//  val day: String = "day21.txt"

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

  val tiles = parse.toSet

  val edgeToTiles: Map[String, Set[Tile]] =
    tiles
      .flatMap(t => t.edges.map(_ -> t))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

  val imageFrame: Set[Tile] = edgeToTiles.filter(_._2.size == 1).flatMap(_._2).toSet

  def solutionPartA: String = {
    val cornerTiles = imageFrame.filter { tile =>
      val otherTiles: Set[Tile] = tiles - tile
      val otherEdges = otherTiles.flatMap(_.edges)
      tile.edges.count(otherEdges.contains) == 4
    }
    cornerTiles.map(_.id.toLong).product.toString
  }

  def solutionPartB: String = ""

  run()

  case class Tile(id: Int, img: Image) {

    private def edgesFrom(img: Image) =
      Set(
        img.head.mkString,
        img.head.reverse.mkString,
        img.last.mkString,
        img.last.reverse.mkString
      )

    val edges: Set[String] = edgesFrom(img) ++ edgesFrom(img.transpose)

    override def toString: String = id.toString
  }
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
