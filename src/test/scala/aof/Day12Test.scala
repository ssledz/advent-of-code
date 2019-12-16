package aof

import aof.Day12.Moon
import aof.Day12Test.MoonsEx1
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  test("step") {
    val ms = Day12.steps(MoonsEx1, 10)
    assert(Day12.totalEnergy(ms) == 179)
  }

  test("reachInitialState ex1") {
    assert(Day12.reachInitialState(Day12Test.MoonsEx1) == 2772)
  }

  test("reachInitialState ex2") {
    assert(Day12.reachInitialState(Day12Test.MoonsEx2) == 4_686_774_924L)
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

  val Ex2 = List(
    "<x=-8, y=-10, z=0>",
    "<x=5, y=5, z=10>",
    "<x=2, y=-7, z=3>",
    "<x=9, y=-8, z=-3>"
  )

  val MoonsEx1 = Day12Test.Ex1.map(Moon.from)

  val MoonsEx2 = Day12Test.Ex2.map(Moon.from)

}