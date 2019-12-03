package aof

import aof.Day3.{createPath, createSteps, intersections, lines}

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

  def intersections(paths: List[List[Path]]): (Option[Int], List[Point]) = {

    val path = paths.toList.flatten

    val zipped = for {
      p1 <- path
      p2 <- path if p1 != p2
    } yield (p1, p2)

    val intersections = zipped.map { case (p1, p2) =>
      p1.intersect(p2)
    }.collect { case Some(a) => a }

    val best = intersections.map(p => Math.abs(p.x) + Math.abs(p.y)).sorted.headOption

    (best, intersections)

  }

  case class Path(start: Point, end: Point) {

    def horizontal: Boolean = start.y == end.y

    def vertical: Boolean = start.x == end.x

    def length: Int = end.x - start.x + end.y - start.y

    def intersect(other: Path): Option[Point] = {

      if (horizontal && other.horizontal || vertical && other.vertical) {
        None
      } else {

        if (horizontal) {

          val otherX = other.start.x

          if (start.x <= otherX && end.x >= otherX) {

            val y = start.y

            if (other.start.y <= y && other.end.y >= y) {
              Some(Point(otherX, y))
            } else {
              None
            }

          } else {
            None
          }

        } else {

          val otherY = other.start.y

          if (start.y <= otherY && end.y >= otherY) {

            val x = start.x

            if (other.start.x <= x && other.end.x >= x) {
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

  println(steps1)
  println(steps2)

  val path1 = createPath(steps1)
  val path2 = createPath(steps2)

  println(path1)
  println(path2)

  println("Checks")

  println("intersections check: " + intersections(createSteps(List("R8,U5,L5,D3", "U7,R6,D4,L4")).map(createPath)))

  println("intersections: " + intersections(List(path1, path2)))

}
