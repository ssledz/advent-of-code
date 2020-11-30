package aof

import aof.Day6.{aOrbits, countOrbits, orbitTransfers}
import aof.Day6Test.{directOrbits, directOrbitsPartB}
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  test("aOrbits") {
    assert(aOrbits(directOrbits, "C") === List("B", "COM"))
    assert(aOrbits(directOrbits, "D") === List("C", "B", "COM"))
    assert(aOrbits(directOrbits, "G") === List("B", "COM"))
    assert(aOrbits(directOrbits, "L") === List("K", "J", "E", "D", "C", "B", "COM"))
    assert(aOrbits(directOrbits, "COM") === List.empty)
    assert(aOrbits(directOrbitsPartB, "L") === List("K", "J", "E", "D", "C", "B", "COM"))
    assert(aOrbits(directOrbitsPartB, "I") === List("D", "C", "B", "COM"))
    assert(aOrbits(directOrbitsPartB, "YOU") === List("K", "J", "E", "D", "C", "B", "COM"))
    assert(aOrbits(directOrbitsPartB, "SUN") === List("I", "D", "C", "B", "COM"))
  }

  test("countOrbits") {
    assert(countOrbits(directOrbits) === 42)
  }

  test("orbitTransfers") {
    assert(orbitTransfers(directOrbitsPartB, "L", "I") === List("K", "J", "E", "D"))
    assert(orbitTransfers(directOrbitsPartB, "YOU", "SUN") === List("K", "J", "E", "D", "I"))
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
    "K)L"
  ))

  val directOrbitsPartB = Day6.newDirectOrbits(List(
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
    "K)L",
    "K)YOU",
    "I)SUN"
  ))

}
