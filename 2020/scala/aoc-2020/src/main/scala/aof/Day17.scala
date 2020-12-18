package aof

object Day17 extends Day with App {

  type Point3D = (Int, Int, Int)
  type Point4D = (Int, Int, Int, Int)
  type Grid[P] = Map[P, Char]
  type Grid3D = Grid[Point3D]
  type Grid4D = Grid[Point4D]

  val day: String = "day17.txt"

  val grid3D: Grid3D = lines.map(_.zipWithIndex).zipWithIndex.flatMap { case (xs, y) => xs.map { case (c, x) => ((x, y, 0), c) } }.toMap

  val sliceX = lines.head.indices
  val sliceY = lines.indices
  val sliceZ = -1 to 1

  val slice = grid3D.keys.toList

  def genSpace(cycle: Int): List[Point3D] =
    for {
      x <- ((sliceX.start - cycle) to (sliceX.end + cycle)).toList
      y <- (sliceY.start - cycle) to (sliceY.end + cycle)
      z <- (sliceZ.start - cycle) to (sliceZ.end + cycle)
    } yield (x, y, z)

  def neighbors(p: Point3D): List[Point3D] = p match {
    case (x, y, z) =>
      val r = (-1 to 1).toList
      for {
        dx <- r
        dy <- r
        dz <- r
        if !(dx == 0 && dy == 0 && dz == 0)
      } yield (x + dx, y + dy, z + dz)
  }

  def isActive(grid: Grid3D, p: Point3D): Boolean = grid.getOrElse(p, '.') == '#'

  def simP[P](genSpace: Int => List[P], neighbors: P => List[P], isActive: (Grid[P], P) => Boolean)(grid: Grid[P], cycle: Int): (Grid[P], Int) = {
    val space = genSpace(cycle + 2)
    val newGrid = space.map { point =>
      val ns = neighbors(point)
      val cnt = ns.count(n => isActive(grid, n))
      if (isActive(grid, point)) {
        (point, if (cnt == 2 || cnt == 3) '#' else '.')
      } else {
        (point, if (cnt == 3) '#' else '.')
      }
    }
    (newGrid.toMap, cycle + 1)
  }

  val sim3D = simP[Point3D](genSpace, neighbors, isActive) _

  def genSpace4D(cycle: Int): List[Point4D] =
    for {
      x <- ((sliceX.start - cycle) to (sliceX.end + cycle)).toList
      y <- (sliceY.start - cycle) to (sliceY.end + cycle)
      z <- (sliceZ.start - cycle) to (sliceZ.end + cycle)
      w <- (sliceZ.start - cycle) to (sliceZ.end + cycle)
    } yield (x, y, z, w)

  def neighbors4D(p: Point4D): List[Point4D] = p match {
    case (x, y, z, w) =>
      val r = (-1 to 1).toList
      for {
        dx <- r
        dy <- r
        dz <- r
        dw <- r
        if !(dx == 0 && dy == 0 && dz == 0 && dw == 0)
      } yield (x + dx, y + dy, z + dz, w + dw)
  }

  def isActive4D(grid: Grid4D, p: Point4D): Boolean = grid.getOrElse(p, '.') == '#'

  val sim4D = simP[Point4D](genSpace4D, neighbors4D, isActive4D) _

  def solutionPartA: String = {
    val grid6 = (1 to 6).foldLeft(grid3D) { (grid, cycle) =>
      sim3D(grid, cycle)._1
    }
    grid6.values.count(_ == '#').toString
  }

  def solutionPartB: String = {
    val grid4D: Grid4D = grid3D.map { case ((x, y, z), c) => ((x, y, z, 0), c) }
    val grid6 = (1 to 6).foldLeft(grid4D) { (grid, cycle) =>
      sim4D(grid, cycle)._1
    }
    grid6.values.count(_ == '#').toString
  }

  def show(grid: Grid3D, z: Int): String =
    sliceY
      .map { y =>
        sliceX.map { x =>
          grid.getOrElse((x, y, z), '.').toString
        }.mkString
      }
      .mkString("\n")

  run()
}
