package aof

import aof.utils._

import scala.annotation.tailrec

object Day04 extends Day with App {

  val day: String = "day04.txt"

  val key = "iwrupvqb"

  @tailrec
  def mine(key: String, acc: Int, prefix: String): (Int, String) = {
    val k = key + acc
    val m = md5(k)
    if (m.startsWith(prefix)) {
      acc -> m
    } else mine(key, acc + 1, prefix)
  }

  def solutionPartA: String = mine(key, 0, "00000")._1.toString

  def solutionPartB: String = mine(key, 0, "000000")._1.toString

  run()
}
