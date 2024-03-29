package aof

object Day11 extends Day with App {

  type Point = (Int, Int)

  type Area = Map[Point, Char]

  type Rule = (Area, Point) => Char => Char

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

  def newRule1(canSee: (Area, Point) => List[Point])(area: Area, p: Point)(c: Char): Char =
    if (c == 'L' && !canSee(area, p).exists(p => area(p) == '#')) '#' else c

  def newRule2(canSee: (Area, Point) => List[Point])(area: Area, p: Point)(c: Char): Char =
    if (c == '#' && canSee(area, p).count(p => area(p) == '#') >= 4) 'L' else c

  def newRule22(canSee: (Area, Point) => List[Point])(area: Area, p: Point)(c: Char): Char =
    if (c == '#' && canSee(area, p).count(p => area(p) == '#') >= 5) 'L' else c

  def canSee1(w: Int, h: Int)(area: Area, p: Point): List[Point] = adjacent(w, h)(p)

  def canSee2(w: Int, h: Int)(area: Area, p: Point): List[Point] = {

    def pred(p: Point): Boolean = area(p) != '.'

    val rx = (p._1 + 1) until w
    val lx = (p._1 - 1) to (0, -1)
    val downY = (p._2 + 1) until h
    val upY = (p._2 - 1) to (0, -1)

    val r = rx.map(x => (x, p._2))
    val l = lx.map(x => (x, p._2))
    val down = downY.map(y => (p._1, y))
    val up = upY.map(y => (p._1, y))

    val rUp = rx.zip(upY)
    val lUp = lx.zip(upY)
    val rDown = rx.zip(downY)
    val lDown = lx.zip(downY)

    List(r, l, up, down, rUp, lUp, rDown, lDown).flatMap(_.find(pred))
  }

  def simulate(area: Area, rule1: Rule, rule2: Rule): Int = {
    // area -> changed
    def iter(area: Area): (Area, Boolean) = {
      val (newArea, changed) = indices.foldLeft((List.empty[(Point, Char)], false)) {
        case ((acc, changed), p) =>
          val rule = rule1(area, p).andThen(rule2(area, p))
          val c = area(p)
          val newC = rule(c)
          ((p, newC) :: acc, changed || newC != c)
      }
      (newArea.toMap, changed)
    }
    def go(area: Area): Area = {
      val (newArea, changed) = iter(area)
      if (changed) go(newArea) else newArea
    }
    val stabilizedArea = go(area)
    stabilizedArea.values.count(_ == '#')
  }

  def solutionPartA: String = simulate(area, newRule1(canSee1(w, h)), newRule2(canSee1(w, h))).toString

  def solutionPartB: String = simulate(area, newRule1(canSee2(w, h)), newRule22(canSee2(w, h))).toString

  def show(area: Area, w: Int, h: Int): String = (0 until h).map(y => (0 until w).map(x => area(x -> y)).mkString).mkString("\n")

  run()
}
