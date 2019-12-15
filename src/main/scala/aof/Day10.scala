package aof

import scala.annotation.tailrec

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

  def detectedAsteroids(allAsteroids: Seq[(Int, Int)], ms: (Int, Int)): Seq[((Double, Double), Double, (Int, Int))] = {

    val (xs, ys) = ms

    allAsteroids.filter(_ != ms).map { case (x, y) =>

      val xx = x.toDouble - xs
      val yy = y.toDouble - ys

      val (v, d) = if (xx == 0) {
        ((Math.signum(yy), 0.0), yy * yy)
      } else {
        val tg = yy / xx
        val x2j = 1 / (1 + Math.pow(tg, 2))
        val xj = Math.sqrt(x2j)
        val yj = Math.sqrt(1 - x2j)
        ((xj + Math.signum(xx), yj * Math.signum(yy)), Math.pow(xj * x, 2) + Math.pow(yj * y, 2))
      }
      (v, d, (x, y))
    }

  }

  def detectDirectLineOfSight(allAsteroids: Seq[(Int, Int)], ms: (Int, Int)): List[(Int, Int)] = {
    detectedAsteroids(allAsteroids, ms).groupBy(_._1).view.mapValues { as =>
      as.sortBy(_._2).head._3
    }.values.toList
  }

  def bestMonitoringStationLocation(map: Vector[Vector[Char]]): ((Int, Int), List[(Int, Int)]) = {
    val all = allAsteroids(map)
    val xs: Seq[((Int, Int), List[(Int, Int)])] = all.map { ms => (ms, detectDirectLineOfSight(all, ms)) }
    xs.sortBy(_._2.length).last
  }

  def solutionPartA: String = {
    val (location, detected) = bestMonitoringStationLocation(map)
    "" + (location, detected.length)
  }

  //  def generateLaserBeam(ms: (Int, Int)): Seq[(Double, Double)] = {
  //    val (xs, ys) = ms
  //    val r = 0 until 360
  //    val d2rad = 2 * Math.PI / 360
  //    r.map(x => x * d2rad).map(rad => (Math.sin(rad) - xs, Math.cos(rad) - ys))
  //  }

  def hitSequence(asteroids: Seq[(Int, Int)], ms: (Int, Int)): List[(Int, Int)] = {

    val (xs, ys) = ms

    val ps: Seq[((Int, Double, Double), (Int, Int))] = asteroids.map { case (x, y) =>

      val xx = x.toDouble - xs
      val yy = y.toDouble - ys

      val q = (xx, yy) match {
        case (x, y) if x >= 0 && y <= 0 => (1, x, y)
        case (x, y) if x >= 0 && y >= 0 => (2, -x, y)
        case (x, y) if x <= 0 && y >= 0 => (3, -x, -y)
        case (x, y) if x <= 0 && y <= 0 => (4, x, -y)
      }

      q -> (x, y)
    }

    val rs = ps.sortWith { case (((q1, x1, y1), _), ((q2, x2, y2), _)) =>
      if (q1 < q2) true
      else if (q1 == q2 && x1 == x2 && y1 <= y2) true
      else if (q1 == q2 && x1 <= x2) true
      else false
    }

    rs.map(_._2).toList

  }

  def vaporizedSeq(map: Vector[Vector[Char]], ms: (Int, Int)): Seq[(Int, Int)] = {

    val all: Seq[(Int, Int)] = allAsteroids(map)

    @tailrec
    def go(asteroids: Seq[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      val detected = detectDirectLineOfSight(asteroids, ms)
      if (detected.isEmpty) {
        acc
      } else
        go(asteroids.filter(x => !(detected contains x)), hitSequence(detected, ms) ::: acc)
    }

    go(all, List.empty).reverse
  }

  def solutionPartB: String = {
    val (ms, _) = bestMonitoringStationLocation(map)
    val (x, y) = vaporizedSeq(map, ms).take(200).last
    "" + (x * 100 + y)
  }

}

object Day10App extends App {
  println("SolutionPartA: " + Day10.solutionPartA)
  //your answer is too low. You guessed 520.
  //your answer is too low. You guessed 614.
  //your answer is too low. You guessed 1002.
  //You guessed 1811
  //  You guessed 629.
  println("SolutionPartB: " + Day10.solutionPartB)
}