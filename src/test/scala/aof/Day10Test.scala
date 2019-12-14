package aof

import java.net.URL

import aof.Day10.{bestMonitoringStationLocation, detectedAsteroids}
import aof.Day10Test._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Test extends AnyFunSuite {

  test("detectedAsteroids for ex1") {
    val map = toMap("ex1")
    val allAsteroids = Day10.allAsteroids(map)
    assert(detectedAsteroids(allAsteroids, (1, 0)).length == 7)
    assert(detectedAsteroids(allAsteroids, (4, 0)).length == 7)
    assert(detectedAsteroids(allAsteroids, (0, 2)).length == 6)
    assert(detectedAsteroids(allAsteroids, (1, 2)).length == 7)
    assert(detectedAsteroids(allAsteroids, (2, 2)).length == 7)
    assert(detectedAsteroids(allAsteroids, (3, 2)).length == 7)
    assert(detectedAsteroids(allAsteroids, (4, 2)).length == 5)
    assert(detectedAsteroids(allAsteroids, (4, 3)).length == 7)
    assert(detectedAsteroids(allAsteroids, (3, 4)).length == 8)
    assert(detectedAsteroids(allAsteroids, (4, 4)).length == 7)
  }

  test("bestMonitoringStationLocation for ex1") {
    val map = toMap("ex1")
    val (ms, asteroids) = bestMonitoringStationLocation(map)
    println(map.map(_.mkString).mkString("\n"))
    println("allLocations: " + Day10.allLocations(map))
    println("allAsteroids: " + Day10.allAsteroids(map))
    println(ms)
    println(asteroids)

    assert(ms == (3, 4))
    assert(asteroids.length == 8)
  }

  testCase("ex2", (5, 8), 33)
  testCase("ex3", (1, 2), 35)
  testCase("ex4", (6, 3), 41)
  testCase("ex5", (11, 13), 210)

  def testCase(name: String, expectedMs: (Int, Int), n: Int): Unit =
    test(s"bestMonitoringStationLocation for $name") {
      val (ms, asteroids) = bestMonitoringStationLocation(toMap(name))
      assert(ms == expectedMs)
      assert(asteroids.length == n)
    }

}

object Day10Test {

  def rs(name: String): URL = getClass.getClassLoader.getResource(s"day10/$name.txt")

  def lines(url: URL): List[String] = Source.fromFile(url.toURI).getLines().toList

  def map(lines: List[String]): Vector[Vector[Char]] = lines.toVector.map(_.toVector)

  val toMap = rs _ andThen lines andThen map
}
