package aof

import aof.Day3.{createPath, createSteps, intersections}

import scala.annotation.tailrec

object Day3 extends Day {
  val day = "03"

  def createSteps(l: List[String]): List[List[Step]] = l.map(_.split(',').toList.map(Step.apply))

  def createPath(steps: List[Step]): List[Path] = {
    @tailrec
    def go(current: Point, steps: List[Step], acc: List[Path]): List[Path] = steps match {
      case h :: t => {
        val end = current + h
        go(end, t, Path(current, end) :: acc)
      }
      case Nil => acc
    }

    go(Point(0, 0), steps, List.empty).reverse
  }

  def intersections(path1: List[Path], path2: List[Path]): (Option[Int], List[Point]) = {

    val zipped = for {
      p1 <- path1
      p2 <- path2
    } yield (p1, p2)

    //    println("zipped: " + zipped)
    //
    //    val test = zipped.map { case (p1, p2) =>
    //      s"${p1.intersect(p2)} =>  ${(p1, p2)})"
    //    }
    //
    //    println("test: " + test.mkString("\n"))

    val intersections = zipped.map { case (p1, p2) =>
      p1.intersect(p2)
    }.collect { case Some(a) if a != Point(0, 0) => a }

    val best = intersections.map(p => Math.abs(p.x) + Math.abs(p.y)).sorted.headOption

    (best, intersections)

  }

  case class Path(start: Point, end: Point) {

    def minX: Int = Math.min(start.x, end.x)

    def minY: Int = Math.min(start.y, end.y)

    def maxX: Int = Math.max(start.x, end.x)

    def maxY: Int = Math.max(start.y, end.y)

    def horizontal: Boolean = start.y == end.y

    def vertical: Boolean = start.x == end.x

    def length: Int = end.x - start.x + end.y - start.y

    def intersect(other: Path): Option[Point] = {

      if (horizontal && other.horizontal || vertical && other.vertical) {
        None
      } else {

        if (horizontal) {

          val otherX = other.start.x

          if (minX <= otherX && maxX >= otherX) {

            val y = start.y

            if (other.minY <= y && other.maxY >= y) {
              Some(Point(otherX, y))
            } else {
              None
            }

          } else {
            None
          }

        } else {

          val otherY = other.start.y

          if (minY <= otherY && maxY >= otherY) {

            val x = start.x

            if (other.minX <= x && other.maxX >= x) {
              Some(Point(x, otherY))
            } else {
              None
            }

          } else {
            None
          }
        }

      }

    }
  }

  case class Point(x: Int, y: Int) {

    def +(s: Step): Point = s.direction match {
      case 'U' => Point(x, y + s.distance)
      case 'D' => Point(x, y - s.distance)
      case 'L' => Point(x - s.distance, y)
      case 'R' => Point(x + s.distance, y)
    }

  }

  case class Step(direction: Char, distance: Int)

  object Step {
    def apply(arg: String): Step = {
      val direction = arg.charAt(0)
      val distance = arg.substring(1).toInt
      new Step(direction, distance)
    }
  }

}

object Day3App extends App {

  val steps1 :: steps2 :: Nil = createSteps(Day3.lines)

  val path1 = createPath(steps1)
  val path2 = createPath(steps2)

//  val steps = createSteps(List("R8,U5,L5,D3", "U7,R6,D4,L4"))
//  val paths = steps.map(createPath)
//  println(steps)
//  println(paths)
//  println("intersections check: " + intersections(paths(0), paths(1)))

  println("intersections: " + intersections(path1, path2))

}
