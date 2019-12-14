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
      val xx = x - xs
      val yy = y - ys

      val a = if (xx != 0) {
        val a = yy.toDouble / xx.toDouble
        val b = y - a * x
        (a, b, Math.signum(xx), 0)
      } else {
        (0, 0, 0, Math.signum(yy))
      }

      (a, xx * xx + yy * yy, (x, y))
    }

//    println("zs: " + zs)
//    println("zs.len: " + zs.length)
//    println("zs.groupBy(_._1):\n" + zs.groupBy(_._1).toList.map(x => "" + x._1 + "=>" + x._2).mkString("\n"))
//    println("zs.groupBy(_._1).len: " + zs.groupBy(_._1).size)

    zs.groupBy(_._1).view.mapValues { as =>
      as.sortBy(_._2).head._3
    }.values.toList

  }

  def bestMonitoringStationLocation(map: Vector[Vector[Char]]): ((Int, Int), List[(Int, Int)]) = {
    val all = allAsteroids(map)
    val xs: Seq[((Int, Int), List[(Int, Int)])] = all.map { ms => (ms, detectedAsteroids(all, ms)) }
    //    println("xs: " + xs.map(x => "" + (x._1, x._2.length)).mkString("\n"))
    //    println("ms locations" + xs.sortBy(_._2.length).map(x => (x._1, x._2.length)))
    xs.sortBy(_._2.length).last
  }

  def solutionPartA: String = {

    //    println("map: [" + map.head.length + "," + map.length + "]")
    //    println("allAsteroids: " + allAsteroids(map).length)

    val (location, detected) = bestMonitoringStationLocation(map)
    "" + (location, detected.length)
  }

  def solutionPartB: String = ""

}

object Day10App extends App {
  //your answer is too low. You guessed 316
  //your answer is too high. You guessed 335
  println("SolutionPartA: " + Day10.solutionPartA)
  println("SolutionPartB: " + Day10.solutionPartB)
}