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

  val mapTileEdges: Set[Tile] = edgeToTiles.filter(_._2.size == 1).flatMap(_._2).toSet

  val mapEdges: Set[String] = edgeToTiles.filter(_._2.size == 1).keys.toSet

  def solutionPartA: String = {
//    println(mapTileEdges)
//    println(tiles -- mapTileEdges)
    val cornerTiles = mapTileEdges.filter { tile =>
      val otherTiles: Set[Tile] = tiles - tile
      val otherEdges = otherTiles.flatMap(_.edges)
      val neighbours = tile.edges.filter(otherEdges.contains)

//      println
//      println("tile: " + tile)
//      println("tile.edges: " + tile.edges)
//      println("other tiles: " + otherTiles)
//      println("neighbours: " + neighbours.flatMap(edgeToTiles.apply))
//      println("neighbours: " + neighbours)
//      println("neighbours cnt: " + neighbours.size)

      tile.edges.count(otherEdges.contains) == 4
//      tile.edges.count(mapEdges.contains) == 2
    }
    println("cornerTiles" + cornerTiles)
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

//    def innerImg: List[String]
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
