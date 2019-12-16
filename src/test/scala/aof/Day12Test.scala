package aof

import aof.Day12.Moon
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  test("step") {

    val moons = Day12Test.Ex1.map(Moon.from)

    println("Step 0")
    println(Day12Test.moonsToString(moons))
    println("Step 1")
    println(Day12Test.moonsToString(Day12.step(moons)))

    println("Step 1")
    println(Day12Test.moonsToString(Day12.steps(moons, 1)))

    println("Step 10")
    val ms = Day12.steps(moons, 10)
    println(Day12Test.moonsToString(ms))
    assert(Day12.totalEnergy(ms) == 179)

  }

}

object Day12Test {

  def moonsToString(moons: Seq[Moon]): String = moons.map(_.toString).mkString("\n")

  val Ex1 = List(
    "<x=-1, y=0, z=2>",
    "<x=2, y=-10, z=-7>",
    "<x=4, y=-8, z=8>",
    "<x=3, y=5, z=-1>"
  )

}