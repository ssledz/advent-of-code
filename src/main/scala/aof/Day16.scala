package aof

import aof.Day16.{solutionPartA, solutionPartB}

object Day16 extends Day {

  val day: String = "16"

  val input = lines.flatMap(_.toList.map(c => c - '0'))

  val basePattern = List(0, 1, 0, -1)

  def repeat(n: Int, pattern: List[Int] = basePattern): RepeatedIntList = new RepeatedIntList(pattern, n)

  def repeat2(n: Int, pattern: List[Int] = basePattern): LazyList[Int] = {

    def go(xs: List[Int]): LazyList[Int] = xs match {
      case Nil => go(pattern)
      case h :: t => LazyList.fill(n)(h) #::: go(t)
    }

    go(pattern)
  }

  def fft(in: List[Int], i: Int): Int =
    in.zip(repeat(i + 1).drop(1)).map { case (a, b) => a * b }
      .sum.toString.toList.last - '0'

  def fft(in: List[Int]): List[Int] = in.zipWithIndex.map { case (_, i) => fft(in, i) }

  def ffts(in: List[Int], n: Int): List[Int] = (1 to n).foldLeft(in) { (in, _) => fft(in) }

  def solutionPartA: String = ffts(input, 100).take(8).map(_.toString).mkString

  def decodeDigit(in: List[Int], offset: Int, repeatIn: Int = 10000): Int = {

    val pattern = repeat(offset, basePattern).drop(offset + 1)

    val start = offset % in.length

    val partialSum = in.drop(start).sum

    val startIndex = offset + (in.length - start)

    val sum = in.sum
    val diff = in.map(_ * -1).sum

    val iter = startIndex / (repeatIn * 2)

    val left = startIndex - iter * (repeatIn * 2)

    //    println("iter: " + iter)
    //    println("left: " + left)

    val res = (sum + diff) * repeatIn * iter + 1 * repeatIn * sum + (left - repeatIn) * diff + partialSum

    res % 10
  }

  def decode(in: List[Int]): List[Int] = {
    val offset = in.take(7).zip(6 to(0, -1)).map { case (x, i) => x * Math.pow(10, i).toInt }.sum
    (0 until 8).map(d => decodeDigit(in, offset + d)).toList
  }

  def solutionPartB: String = decode(input).map(_.toString).mkString

  class RepeatedIntList(xs: List[Int], n: Int) extends Iterator[Int] {

    private var state = xs

    private var it = n

    def hasNext: Boolean = true

    override def drop(nn: Int): RepeatedIntList = {
      super.drop(nn % (n * xs.length))
      this
    }

    def takeN(nn: Int): List[((Int, Int), Int)] = {

      def go(ys: List[Int], it: Int = 0, acc: List[((Int, Int), Int)] = List.empty): List[((Int, Int), Int)] = {
        if (it == nn) {
          acc
        } else {
          val nextIt = nn min (it + n)
          ys match {
            case h :: t => go(t, nextIt, ((it, nextIt), h) :: acc)
            case Nil => go(xs.tail, nextIt, ((it, nextIt), xs.head) :: acc)
          }
        }
      }


      if (it == n) {
        go(state).reverse
      } else {
        val value = state.head
        if (nn <= it) {
          List(((0, nn), value))
        } else {
          if (it == 0) {
            go(nextState, it).reverse
          } else ((0, it), value) :: go(nextState, it).reverse
        }


      }

    }

    private def nextState = if (state.tail == Nil) xs else state.tail

    def next(): Int = if (it > 0) {
      it = it - 1
      state.head
    } else {
      it = n
      state = nextState
      next
    }
  }

}

object Day16App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
