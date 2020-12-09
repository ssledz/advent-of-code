package aof

import scala.annotation.tailrec

object Day09 extends Day with App {

  val day: String = "day09.txt"

  val numbers = lines.map(_.toLong).toVector

  val size = 25

  def sumOfTwo(xs: Vector[Long], n: Long): Boolean = xs.exists(x => xs.exists(y => x + y == n))

  def notMatched: Vector[Long] = {
    val (preamble, rest) = numbers.splitAt(size)
    @tailrec
    def go(preamble: Vector[Long], rest: Vector[Long], acc: Vector[Long]): Vector[Long] = (preamble, rest) match {
      case (_ +: p, h +: t) => go(p :+ h, t, if (!sumOfTwo(preamble, h)) acc :+ h else acc)
      case _                => acc
    }
    go(preamble, rest, Vector.empty)
  }

  def solutionPartA: String = notMatched.headOption.map(_.toString).getOrElse("")

  def solutionPartB: String = {
    val num = notMatched.head
    def find(size: Int): Vector[Long] = {
      val (preamble, rest) = numbers.splitAt(size)
      @tailrec
      def go(preamble: Vector[Long], rest: Vector[Long]): Vector[Long] = (preamble, rest) match {
        case _ if preamble.sum == num => preamble
        case (_ +: p, h +: t)         => go(p :+ h, t)
        case _                        => Vector.empty
      }
      go(preamble, rest)
    }
    val rs = (2 until numbers.size).toList
    val maybeSeq = rs.map(r => find(r)).find(_.nonEmpty)
    maybeSeq.map(xs => xs.min + xs.max).map(_.toString).getOrElse("")
  }

  run()
}