package aof

object Day22 extends Day with App {

  val day: String = "day22.txt"

  def parseDecks: (List[Int], List[Int]) = {
    assert(lines.head == "Player 1:")
    val (p1, rest) = lines.tail.span(!_.isBlank)
    assert(rest.tail.head == "Player 2:")
    (p1.map(_.toInt), rest.tail.tail.map(_.toInt))
  }

  val (p1, p2) = parseDecks

  def solutionPartA: String = {
    def play(p1: List[Int], p2: List[Int]): List[Int] = (p1, p2) match {
      case (h1 :: t1, h2 :: t2) if h1 > h2 => play(t1 ::: List(h1, h2), t2)
      case (h1 :: t1, h2 :: t2) if h1 < h2 => play(t1, t2 ::: List(h2, h1))
      case (d, Nil)                        => d
      case (Nil, d)                        => d
    }
    val wd = play(p1, p2)
    (1 to wd.length).reverse.zip(wd).foldLeft(0L) { case (acc, (l, r)) => acc + l * r }.toString
  }

  def solutionPartB: String =
    ""

  run()
}
