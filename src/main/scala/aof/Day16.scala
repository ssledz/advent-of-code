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
    in.zipWithIndex.map { case (_, i) =>
      val x = in.zip(repeat(i + 1).drop(1)).map { case (a, b) => a * b }.sum
      x.toString.toList.last - '0'
    }
  }

  def ffts(in: List[Int], n: Int): List[Int] = (1 to n).foldLeft(in) { (in, _) => fft(in) }

  def solutionPartA: String = ffts(input, 100).take(8).map(_.toString).mkString

  def solutionPartB: String = ""

}

object Day16App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
