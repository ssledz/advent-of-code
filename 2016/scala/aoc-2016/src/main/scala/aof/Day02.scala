package aof

import aof.utils._

object Day02 extends Day with App {

  val day: String = "day02.txt"

  val keypad = (1 to 9).map(_.toString).toVector

  val moves = Map(('U', (0, -1)), ('D', (0, 1)), ('L', (-1, 0)), ('R', (1, 0)))

  val ins = lines.map(_.toList)

  def key(keypad: Vector[String], w: Int)(x: Int, y: Int): String = keypad(y * w + x)

  def run(start: (Int, Int), f: ((Int, Int), (Int, Int)) => (Int, Int), key: (Int, Int) => String): String = {
    val (_, xs) = ins.foldLeft(start, List.empty[(Int, Int)]) {
      case ((startPos, acc), ys) =>
        val zs = ys.map(moves.apply).foldLeft(startPos) { (pos, dPos) =>
          f(pos, dPos)
        }
        (zs, zs :: acc)
    }
    xs.reverse.map(key.tupled).mkString
  }

  def solutionPartA: String = run((1, 1), (pos, dPos) => (pos + dPos).min(0, 0).max(2, 2), key(keypad, 3))

  def solutionPartB: String = {
    //@formatter:off
    val keypad2 = Vector(
      " ", " ", "1", " ", " ",
      " ", "2", "3", "4", " ",
      "5", "6", "7", "8", "9",
      " ", "A", "B", "C", " ",
      " ", " ", "D", " ", " ",
    )
    val forbidden = Set(
      (0,0), (1,0), (3,0), (4,0),
      (0,1), (4,1),
      (0,3), (4,3),
      (0,4), (1,4), (3,4), (4,4)
    )
    //@formatter:on
    def doMove(pos: (Int, Int), dPos: (Int, Int)): (Int, Int) = {
      val next = pos + dPos
      if (forbidden.contains(next)) pos else next.min(0, 0).max(4, 4)
    }

    run((0, 2), doMove, key(keypad2, 5))
  }

  run()
}
