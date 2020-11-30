package aof

import aof.Day3.{Point, bestSteps, createPath, createSteps, intersections, pathTo}
import aof.Day3Test._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  test("intersections ex1") {

    val actual = intersectionsFrom(Ex1)

    assert(actual._2 contains Point(3, 3))

    assert(actual._1 === Some(6))

  }

  test("intersections ex2") {

    val actual = intersectionsFrom(Ex2)

    assert(actual._1 === Some(159))

  }

  test("intersections ex3") {

    val actual = intersectionsFrom(Ex3)

    assert(actual._1 === Some(135))

  }

  test("best steps ex1") {
    bestStepTest(Ex1, 30)
  }

  test("best steps ex2") {
    bestStepTest(Ex2, 610)
  }

  test("best steps ex3") {
    bestStepTest(Ex3, 410)
  }

  def bestStepTest(ex: List[String], required: Int): Unit = {

    val path1 :: path2 :: Nil = createSteps(ex).map(createPath)

    val (_, xs) = intersections(path1, path2)

    val actual = bestSteps(xs, path1, path2)

    assert(actual === required)

  }

}

object Day3Test {

  val Ex1 = List("R8,U5,L5,D3", "U7,R6,D4,L4")
  val Ex2 = List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
  val Ex3 = List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  def intersectionsFrom(xs: List[String]): (Option[Int], List[Point]) = {
    val path1 :: path2 :: Nil = createSteps(xs).map(createPath)
    intersections(path1, path2)
  }

}
