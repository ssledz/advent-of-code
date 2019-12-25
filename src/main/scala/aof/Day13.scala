package aof

import aof.Day13.solutionPartB

object Day13 extends Day {

  val day: String = "13"

  object Tile {
    val Empty = 0 -> " "
    val Wall = 1 -> "#"
    val Block = 2 -> "*"
    val HorizontalPaddle = 3 -> "|"
    val Ball = 4 -> "o"

    val byId = Map(Empty, Wall, Block, HorizontalPaddle, Ball)

  }

  def memory: Array[Long] = lines.head.split(',').map(_.toLong)

  def solutionPartA: String = {
    "" + IntComputer(memory).extendMemory().runInterpreter(List.empty).output
      .sliding(3, 3)
      .map(x => x.head)
      .count(_ == Tile.Block._1)
  }

  def buildMap(xs: List[Long]): (Map[(Int, Int), Int], String) = {

    val map: Map[(Int, Int), Int] = Map(xs.reverse.sliding(3, 3).map(x => ((x(0).toInt, x(1).toInt), x(2).toInt)).toSeq: _*)

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

  def solutionPartB: String = {

    val lMemory = memory
    lMemory(0) = 2

    val c = IntComputer(lMemory).extendMemory().runInterpreter(List.empty)

    val (map, mapStr) = buildMap(c.output)

    println(mapStr)

    println(c.output)

    val c2 = c.copy(debug = false, trace = false).runInterpreter(List(-1))

    println("waitingInput: " + c2.waitingInput)

    val (map2, mapStr2) = buildMap(c2.output)

    "\n" + mapStr2
  }

}

object Day13App extends App {
  //  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
