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

  def fft(in: List[Int], i: Int): Int =
    in.zip(repeat(i + 1).drop(1)).map { case (a, b) => a * b }
      .sum.toString.toList.last - '0'

  def fft(in: List[Int]): List[Int] = in.zipWithIndex.map { case (_, i) => fft(in, i) }

  def ffts(in: List[Int], n: Int): List[Int] = (1 to n).foldLeft(in) { (in, _) => fft(in) }

  def solutionPartA: String = ffts(input, 100).take(8).map(_.toString).mkString

  def decodeDigit(in: List[Int], offset: Int, repeatIn: Int = 10000): Int = {

    val pattern = repeat(offset, basePattern).drop(offset + 1)

    val start = offset % in.length

    val partialSum = in.drop(start).sum

    val startIndex = offset + (in.length - start)

    val sum = in.sum
    val diff = in.map(_ * -1).sum

    val iter = startIndex / (repeatIn * 2)

    val left = startIndex - iter * (repeatIn * 2)

//    println("iter: " + iter)
//    println("left: " + left)

    val res = (sum + diff) * repeatIn * iter + 1 * repeatIn * sum + (left - repeatIn) * diff + partialSum

    res % 10
  }

  def decode(in: List[Int]): List[Int] = {
    val offset = in.take(7).zip(6 to(0, -1)).map { case (x, i) => x * Math.pow(10, i).toInt }.sum
    (0 until 8).map(d => decodeDigit(in, offset + d)).toList
  }

  def solutionPartB: String = decode(input).map(_.toString).mkString

}

object Day16App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
