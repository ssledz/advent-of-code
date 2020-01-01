package aof

import aof.Day16.{solutionPartA, solutionPartB}

object Day16 extends Day {

  val day: String = "16"

  val input = lines.flatMap(_.toList.map(c => c - '0'))

  val basePattern = List(0, 1, 0, -1)

  def repeat(n: Int, pattern: List[Int] = basePattern): LazyList[Int] = {

    def go(xs: List[Int]): LazyList[Int] = xs match {
      case Nil => go(pattern)
      case h :: t => LazyList.fill(n)(h) #::: go(t)
    }

    go(pattern)
  }

  def fft(in: List[Int]): List[Int] = {

    //    in.zipWithIndex
    ???

  }

  def solutionPartA: String = {
    val xs = List()
    println(repeat(2).drop(1).take(10).toList)
    ""
  }

  def solutionPartB: String = ""

}

object Day16App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
