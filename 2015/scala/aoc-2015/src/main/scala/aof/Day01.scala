package aof

object Day01 extends Day with App {

  val day: String = "day01.txt"

  val line = lines.mkString

  def solutionPartA: String = {
    val xs = line.toCharArray.foldLeft(0) { (acc, x) =>
      if (x == '(') acc + 1 else acc - 1
    }
    xs.toString
  }

  def solutionPartB: String = {
    def go(xs: List[Char], floor: Int, position: Int): Int = xs match {
      case _ if floor == -1 => position
      case h :: t           => go(t, floor + (if (h == '(') 1 else -1), position + 1)
      case Nil              => -1
    }
    go(line.toCharArray.toList, 0, 0).toString
  }

  run()
}
