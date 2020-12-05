package aof

object Day05 extends Day with App {

  val day: String = "day05.txt"

  val seats = lines.map(decodeSeat)

  def decode(cs: List[Char], l: Int, r: Int, cl: Char, cr: Char): Int = cs match {
    case c :: Nil if c == cl => l
    case c :: Nil if c == cr => r
    case c :: t if c == cl => decode(t, l, l + (r - l) / 2, cl, cr)
    case c :: t if c == cr => decode(t, l + (r - l) / 2 + 1, r, cl, cr)
  }

  def decodeSeat(code: String): (Int, Int) = {
    def decodeRow(cs: List[Char], l: Int, r: Int): Int = decode(cs, l, r, 'F', 'B')

    def decodeCol(cs: List[Char], l: Int, r: Int): Int = decode(cs, l, r, 'L', 'R')

    (decodeRow(code.take(7).toCharArray.toList, 0, 127), decodeCol(code.drop(7).toCharArray.toList, 0, 7))
  }

  def solutionPartA: String = seats.map { case (row, col) => row * 8 + col }.max.toString

  def solutionPartB: String = {
    val maxSeatId = 128 * 8 - 1
    val all = (0 to maxSeatId).toSet
    val bps = seats.map { case (row, col) => row * 8 + col }
    val remaining = bps.foldLeft(all)((acc, id) => acc - id)
    val bpsSet = bps.toSet
    val xs = remaining.filter(id => bpsSet.contains(id - 1) && bpsSet.contains(id + 1))
    xs.toString
  }

  run()
}
