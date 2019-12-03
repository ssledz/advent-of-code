package aof

import aof.Day3.{Point, createPath, createSteps, intersections}
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  test("intersections ex1") {

    val path1 :: path2 :: Nil = createSteps(List("R8,U5,L5,D3", "U7,R6,D4,L4")).map(createPath)

    val actual = intersections(path1, path2)

    println(actual)

    assert(actual._2 contains Point(3, 3))
    assert(actual._1 === Some(6))

  }

  test("intersections ex2") {

    val xs = List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")

    val path1 :: path2 :: Nil = createSteps(xs).map(createPath)

    val actual = intersections(path1, path2)

    println(actual)

    assert(actual._1 === Some(159))

  }

  test("intersections ex3") {

    val xs = List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

    val path1 :: path2 :: Nil = createSteps(xs).map(createPath)

    val actual = intersections(path1, path2)

    println(actual)

    assert(actual._1 === Some(135))

  }

}
