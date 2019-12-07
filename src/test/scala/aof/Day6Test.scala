package aof

import aof.Day6.{aOrbits, countOrbits}
import aof.Day6Test.directOrbits
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  test("aOrbits") {
    assert(aOrbits(directOrbits, "C") === List("B", "COM"))
    assert(aOrbits(directOrbits, "D") === List("C", "B", "COM"))
    assert(aOrbits(directOrbits, "G") === List("B", "COM"))
    assert(aOrbits(directOrbits, "L") === List("K", "J", "E", "D", "C", "B", "COM"))
    assert(aOrbits(directOrbits, "COM") === List.empty)
  }

  test("countOrbits") {
    assert(countOrbits(directOrbits) === 42)
  }

}

object Day6Test {

  val directOrbits = Day6.newDirectOrbits(List(
    "COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L"))

}
