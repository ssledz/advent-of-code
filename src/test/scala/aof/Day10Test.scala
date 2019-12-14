package aof

import java.net.URL

import aof.Day10.bestMonitoringStationLocation
import aof.Day10Test._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Test extends AnyFunSuite {

  test("bestMonitoringStationLocation for ex1") {
    val (ms, asteroids) = bestMonitoringStationLocation(toMap("ex1"))
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
