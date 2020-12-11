package aof

object Day11 extends Day with App {

  type Point = (Int, Int)

  val day: String = "day11.txt"

  val area = lines.map(_.toList.zipWithIndex).zipWithIndex.flatMap { case (ys, y) => ys.map { case (c, x) => ((x, y), c) } }.toMap

  val indices = area.keys.toList

  val w = indices.maxBy(x => x._1)._1 + 1

  val h = indices.maxBy(x => x._2)._2 + 1

  def adjacent(w: Int, h: Int)(p: Point): List[Point] = p match {
    case (x, y) =>
      val xs = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x + 1, y + 1), (x - 1, y + 1), (x - 1, y - 1), (x + 1, y - 1))
      xs.filterNot {
        case (x, y) => x < 0 || y < 0 || x >= w || y >= h
      }
  }

  def newRule1(adjacent: Point => List[Point])(area: Map[Point, Char], p: Point)(c: Char): Char =
    if (c == 'L' && !adjacent(p).exists(p => area(p) == '#')) '#' else c

  def newRule2(adjacent: Point => List[Point])(area: Map[Point, Char], p: Point)(c: Char): Char =
    if (c == '#' && adjacent(p).count(p => area(p) == '#') >= 4) 'L' else c

  def solutionPartA: String = {
    val getAdjacent = adjacent(w, h) _
    val rule1 = newRule1(getAdjacent) _
    val rule2 = newRule2(getAdjacent) _

    // area -> changed
    def iter(area: Map[Point, Char]): (Map[Point, Char], Boolean) = {
      val (newArea, changed) = indices.foldLeft((List.empty[(Point, Char)], false)) {
        case ((acc, changed), p) =>
          val rule = rule1(area, p).andThen(rule2(area, p))
          val c = area(p)
          val newC = rule(c)

//          println(s"($p)\t\t$c\t->\t$newC\t\trule1: ${rule1(area, p)(c)}\t\trule2: ${newC}")

          ((p, newC) :: acc, changed || newC != c)
      }
      (newArea.toMap, changed)
    }

    def go(area: Map[Point, Char]): Map[Point, Char] = {
      val (newArea, changed) = iter(area)
      if (changed) go(newArea) else newArea
    }

    val stabilizedArea = go(area)
    stabilizedArea.values.count(_ == '#').toString

//    val (newArea, changed) = iter(area)
//    val (newArea2, changed2) = iter(newArea)
//    println(show(area, w, h))
//    println(s"\nchanged: $changed\n")
//    println(show(newArea, w, h))
//    println(s"\nchanged: $changed2\n")
//    println(show(newArea2, w, h))

  }

  def solutionPartB: String = ""

  def show(area: Map[Point, Char], w: Int, h: Int): String = (0 until h).map(y => (0 until w).map(x => area(x -> y)).mkString).mkString("\n")

  run()
}
