package aof

import scala.annotation.tailrec

object Day23 extends Day with App {

  val day: String = "day23.txt"

  val cups = lines.head.toVector.map(_.toString.toInt)

  def solutionPartA: String = {
    def take(xs: Vector[Int], from: Int, n: Int): Vector[Int] = {
      val ys = xs.drop(from).take(n)
      if (ys.size == n) ys else ys ++ xs.take(n - ys.size)
    }

    @tailrec
    def pickDest(pics: Vector[Int], dest: Int): Int =
      if (dest < 1) pickDest(pics, 9)
      else if (!pics.contains(dest)) dest
      else pickDest(pics, dest - 1)

    def addAfter(to: Vector[Int], what: Vector[Int], after: Int, curr: Int): (Vector[Int], Int) = {
      val destIdx = to.indexOf(after)
      val (l, r) = to.splitAt(destIdx + 1)
      val res = l ++ what ++ r
      (res, res.zipWithIndex.find(_._1 == curr).get._2)
    }

    def play(cups: Vector[Int], n: Int): (Vector[Int], Int) = {
      val curr = cups(n % cups.size)
      val from = (n + 1) % cups.size
      val pics = take(cups, from, 3)

      val dest: Int = pickDest(pics, curr - 1)
//      println("cups: " + show(cups, currIdx))
//      println("pick up: " + pics.mkString(", "))
//      println("destination: " + dest)
      val dropped = cups.filterNot(pics.contains)
      val (newCups, nextCurrIdx) = addAfter(dropped, pics, dest, curr)
      (newCups, nextCurrIdx + 1)
    }

    def playN(xs: Vector[Int], n: Int): Vector[Int] =
      (1 to n)
        .foldLeft((xs, 0)) {
          case ((cups, n), turn) =>
//            println(s"\n-- move $turn --")
            play(cups, n)
        }
        ._1

    val cupsN = playN(cups, 100)

    (cupsN.dropWhile(_ != 1).tail ++ cupsN.takeWhile(_ != 1)).mkString
  }

  def show(xs: Vector[Int], n: Int): String =
    xs.zipWithIndex
      .map {
        case (x, i) if i == n => s"($x)"
        case (x, _)           => x.toString
      }
      .mkString(" ")

  def solutionPartB: String =
    ""

  run()
}
