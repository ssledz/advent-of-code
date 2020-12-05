package aof

object Day02 extends Day with App {

  val day: String = "day02.txt"

  val dims = lines.map(_.split('x').map(_.toInt).toList)

  def areas(l: Int, w: Int, h: Int): List[Int] = List(l * w, w * h, h * l)

  def solutionPartA: String = {
    val xs = dims.map {
      case l :: w :: h :: Nil =>
        val as = areas(l, w, h)
        as.foldLeft(0)(_ + 2 * _) + as.min
    }
    xs.sum.toString
  }

  def solutionPartB: String = {
    val xs = dims.map { dim =>
      dim.sorted.take(2).sum * 2 + dim.product
    }
    xs.sum.toString
  }

  run()
}
