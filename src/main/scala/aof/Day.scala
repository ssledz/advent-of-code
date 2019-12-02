package aof

import aof.Day.IdOps

import scala.io.Source

trait Day {

  val day: String

  private lazy val rs = getClass.getClassLoader.getResource(s"input/day$day.txt")

  lazy val lines = Source.fromFile(rs.toURI).getLines().toList

  implicit def idOps[A](a: A): IdOps[A] = IdOps(a)

}

object Day {

  case class IdOps[A](a: A) extends AnyVal {

    def ===(b: A): Unit = {
      if (a != b) {
        println(s"Required : $b but is $a")
      }
      assert(a == b)
    }

  }

}