package aof

object Day10 extends Day {

  val day: String = "10"

  val map = lines.toVector.map(_.toVector)

  val Asteroid = '#'

  def allLocations(map: Vector[Vector[Char]]): Seq[(Int, Int)] = {
    val xr = 0 until map.head.length
    val yr = 0 until map.length
    for {
      x <- xr
      y <- yr
    } yield (x, y)
  }

  def allAsteroids(map: Vector[Vector[Char]]): Seq[(Int, Int)] = allLocations(map).filter { case (x, y) =>
    map(y)(x) == Asteroid
  }

  def detectedAsteroids(allAsteroids: Seq[(Int, Int)], ms: (Int, Int)): List[(Int, Int)] = {

    val (xs, ys) = ms

    val zs = allAsteroids.filter(_ != ms).map { case (x, y) =>

      val xx = x.toDouble - xs
      val yy = y.toDouble - ys

      val (v, d) = if (xx == 0) {
        ((Math.signum(yy), 0), yy * yy)
      } else {
        val tg = yy / xx
        val x2j = 1 / (1 + Math.pow(tg, 2))
        val xj = Math.sqrt(x2j)
        val yj = Math.sqrt(1 - x2j)
        ((xj + Math.signum(xx), yj * Math.signum(yy)), Math.pow(xj * x, 2) + Math.pow(yj * y, 2))
      }
      (v, d, (x, y))
    }

    zs.groupBy(_._1).view.mapValues { as =>
      as.sortBy(_._2).head._3
    }.values.toList

  }

  def bestMonitoringStationLocation(map: Vector[Vector[Char]]): ((Int, Int), List[(Int, Int)]) = {
    val all = allAsteroids(map)
    val xs: Seq[((Int, Int), List[(Int, Int)])] = all.map { ms => (ms, detectedAsteroids(all, ms)) }
    xs.sortBy(_._2.length).last
  }

  def solutionPartA: String = {
    val (location, detected) = bestMonitoringStationLocation(map)
    "" + (location, detected.length)
  }

  def solutionPartB: String = ""

}

object Day10App extends App {
  println("SolutionPartA: " + Day10.solutionPartA)
  println("SolutionPartB: " + Day10.solutionPartB)
}