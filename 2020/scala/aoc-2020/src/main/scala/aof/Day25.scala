package aof

import aof.utils._

import scala.collection.mutable
object Day25 extends Day with App {

  val day: String = "day25.txt"
//  val day: String = "dayXX.txt"

  val cardPK = lines.head.toInt
  val doorPK = lines.tail.head.toInt
  val initSub = 7
  val mod = 20201227

  implicit val m = mutable.Map.empty[Int, Long]

  def tran(sub: Int, loopSize: Int): Int = {
    def steps(acc: Int): Int = ((acc.toLong * sub) % mod).toInt
    (1 to loopSize).foldLeft(1)((acc, _) => steps(acc))
  }

  def guessLoopSize(pk: Int): Int =
    LazyList.iterate(1)(_ + 1).find(n => powMod(initSub, n, mod).toInt == pk).head

  lazy val doorLoopSize = guessLoopSize(doorPK)
  lazy val cardLoopSize = guessLoopSize(cardPK)
  val encKey = tran(doorPK, cardLoopSize)

  def solutionPartA: String = {
//    println("card loopSize: " + cardLoopSize)
//    println("door loopSize: " + doorLoopSize)
//    val encKey = tran(doorPK, cardLoopSize)
    assert(encKey == tran(cardPK, doorLoopSize))
    encKey.toString
  }

  def solutionPartB: String = {
    ""
    ""
  }

  run()
}
