package aof

import scala.io.Source

trait Day {

  val day: String

  private val rs = getClass.getClassLoader.getResource(s"input/day$day.txt")

  val lines = Source.fromFile(rs.toURI).getLines().toList

}
