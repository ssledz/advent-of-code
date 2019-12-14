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

  // y = ax => a = y / x
  def detectedAsteroids(allAsteroids: Seq[(Int, Int)], ms: (Int, Int)): List[(Int, Int)] = {

    val (xs, ys) = ms

    val zs = allAsteroids.map { case (x, y) =>
      val xx = x - xs
      val yy = y - ys
      val a = if (xx == 0) 0 else yy.toDouble / xx.toDouble
      (a, xx * xx + yy * yy, (x, y))
    }

    zs.groupBy(_._1).view.mapValues { as =>
      as.sortBy(_._2).head._3
    }.values.toList

  }

  def bestMonitoringStationLocation(map: Vector[Vector[Char]]): ((Int, Int), List[(Int, Int)]) = {
    val all = allAsteroids(map)
    val xs = all.map { ms => (ms, detectedAsteroids(all.filter(_ != ms), ms)) }
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
  println("SolutionPartA: " + Day10.solutionPartA)
  println("SolutionPartB: " + Day10.solutionPartB)
}