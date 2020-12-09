package aof

object Day01 extends Day with App {

  val day: String = "day01.txt"

  val freqs = lines.map(_.toInt)

  def solutionPartA: String = freqs.sum.toString

  def solutionPartB: String = {
    lazy val infFreqs: LazyList[Int] = LazyList.from(freqs) #::: infFreqs
    infFreqs
      .scanLeft((Set.empty[Int], 0, true)) { case ((xs, acc, _), e) => (xs + acc, acc + e, !xs.contains(acc)) }
      .takeWhile(_._3)
      .last
      ._2
      .toString
  }

  run()
}
