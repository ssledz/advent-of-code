package aof

import aof.Day13.solutionPartB

import scala.annotation.tailrec

object Day13 extends Day {

  val day: String = "13"

  object Tile {
    val Empty = 0 -> " "
    val Wall = 1 -> "#"
    val Block = 2 -> "*"
    val HorizontalPaddle = 3 -> "-"
    val Ball = 4 -> "o"

    val byId = Map(Empty, Wall, Block, HorizontalPaddle, Ball)

  }

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  def solutionPartA: String = {
    "" + IntComputer(memory.toArray).extendMemory().runInterpreter(List.empty).output
      .sliding(3, 3)
      .map(x => x.head)
      .count(_ == Tile.Block._1)
  }

  def toTileMap(xs: List[Long]): Map[(Int, Int), Int] =
    Map(xs.reverse.sliding(3, 3).map(x => ((x(0).toInt, x(1).toInt), x(2).toInt)).toSeq: _*)

  def writeMap(xs: List[Long]): (Map[(Int, Int), Int], String) = {

    val map: Map[(Int, Int), Int] = toTileMap(xs)

    val (maxX, maxY) = map.keys.foldLeft((Int.MinValue.toInt, Int.MinValue.toInt)) { case ((maxX, maxY), (x, y)) =>
      (maxX max x, maxY max y)
    }

    val str = (for (y <- 0 to maxY) yield {
      (0 to maxX).map(x => Tile.byId(map((x, y)))).mkString
    }).mkString("\n")

    val ball = map.find(x => x._2 == Tile.Ball._1).get._1
    val hp = map.count(x => x._2 == Tile.HorizontalPaddle._1)
    val ws = map.count(x => x._2 == Tile.Wall._1)
    val empty = map.count(x => x._2 == Tile.Empty._1)
    val display = map.get((-1, 0)).getOrElse(0)

    val info =
      s"""
         | (maxX, maxY) : ($maxX, $maxY)
         | ball : $ball
         | horizontal paddles : $hp
         | walls : $ws
         | empty : $empty
         | score: $display
         |""".stripMargin

    (map, info + "\n" + str)
  }

  def initMemory: Array[Long] = {
    val m = memory.toArray
    m(0) = 2
    m
  }

  def playGame: String = {

    def score(map: Map[(Int, Int), Int]): Int =
      map.get((-1, 0)).getOrElse(0)

    def numberOfBlocks(map: Map[(Int, Int), Int]): Int =
      map.count(x => x._2 == Tile.Block._1)

    @tailrec
    def go(moves: List[List[Int]], best: (List[Int], Int)): List[Int] = moves match {
      case h :: t => {
        val c = IntComputer(initMemory).extendMemory().runInterpreter(h)
        val tm = toTileMap(c.output)
        val nob = numberOfBlocks(tm)
        if (nob == 0) {
          h
        } else if (!c.waitingInput) {
          go(t, if (nob < best._2) (h, nob) else best)
        } else {
          go((h :+ 0) :: (h :+ -1) :: (h :+ 1) :: t, if (nob < best._2) (h, nob) else best)
        }
      }
      case Nil => best._1
    }

    val xs = go(List(List(0), List(-1), List(1)), (List.empty, Int.MaxValue))

    val c = IntComputer(initMemory).extendMemory().runInterpreter(xs)

    val (m, mStr) = writeMap(c.output)

    //score(m)
    s"\n moves: ${xs}\n" + mStr

  }

  def solutionPartB: String = {
    "" + playGame
  }

}

object Day13App extends App {
  //  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
