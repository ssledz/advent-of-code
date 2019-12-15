package aof

import java.net.URL

import aof.Day10.{bestMonitoringStationLocation, detectDirectLineOfSight, vaporizedSeq}
import aof.Day10Test._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Test extends AnyFunSuite {

  test("detectedAsteroids for ex1") {
    val map = toMap("ex1")
    val allAsteroids = Day10.allAsteroids(map)
    assert(detectDirectLineOfSight(allAsteroids, (1, 0)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (4, 0)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (0, 2)).length == 6)
    assert(detectDirectLineOfSight(allAsteroids, (1, 2)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (2, 2)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (3, 2)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (4, 2)).length == 5)
    assert(detectDirectLineOfSight(allAsteroids, (4, 3)).length == 7)
    assert(detectDirectLineOfSight(allAsteroids, (3, 4)).length == 8)
    assert(detectDirectLineOfSight(allAsteroids, (4, 4)).length == 7)
  }

  test("bestMonitoringStationLocation for ex1") {
    val map = toMap("ex1")
    val (ms, asteroids) = bestMonitoringStationLocation(map)
    assert(ms == (3, 4))
    assert(asteroids.length == 8)
  }

  testCase("ex2", (5, 8), 33)
  testCase("ex3", (1, 2), 35)
  testCase("ex4", (6, 3), 41)
  testCase("ex5", (11, 13), 210)

  test("vaporizedSeq for ex5") {
    val map = toMap("ex5")
    val (ms, _) = bestMonitoringStationLocation(map)
    val xs = vaporizedSeq(map, ms)
    assert(ms == (11, 13))
//    print("xs: " + xs)
    assert(xs.head == (11, 12))
    assert(xs(1) == (12, 1))
    assert(xs(2) == (12, 2))
    assert(xs.take(3).last == (12, 2))
    assert(xs.take(10).last == (12, 8))
    assert(xs.take(20).last == (16, 0))
    assert(xs.take(50).last == (16, 9))
    assert(xs.take(100).last == (10, 16))
    assert(xs.take(199).last == (9, 6))
    assert(xs.take(200).last == (8, 2))
    assert(xs.take(201).last == (10, 9))
    assert(xs.take(299).last == (11, 1))
  }

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
