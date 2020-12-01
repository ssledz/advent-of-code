package aof

import scala.io.Source

trait Day {

  val day: String

  private lazy val rs = getClass.getClassLoader.getResource(s"input/$day")

  lazy val lines = Source.fromFile(rs.toURI).getLines().toList

  def solutionPartA: String

  def solutionPartB: String

  def run(): Unit = {
    println("SolutionPartA: " + solutionPartA)
    println("SolutionPartB: " + solutionPartB)
  }
}

