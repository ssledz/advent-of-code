package aof

object Day8 extends Day {

  val day: String = "08"

  private def toInt(c: Char): Int = c - '0'

  val image = lines.flatMap(_.toCharArray).map(toInt)

  val width: Int = 25
  val tall: Int = 6

  val Black = 0
  val White = 1
  val Transparent = 2

  def toLayers(img: List[Int], w: Int, h: Int): List[List[Int]] = {

    def go(i: Int, acc: List[List[Int]]): List[List[Int]] = {
      if (img.length == i) {
        acc
      } else {
        go(i + w * h, img.slice(i, i + w * h) :: acc)
      }
    }

    go(0, List.empty).reverse
  }

  val layers: List[List[Int]] = toLayers(image, width, tall)

  def solutionPartA: String = {

    val (_, x) = layers.map(xs => (xs.count(_ == 0), xs)).sortBy(_._1).head

    val ret = x.count(_ == 1) * x.count(_ == 2)

    "" + ret
  }

  def stackLayers(layers: List[List[Int]]): List[List[Int]] = {
    val zero = layers.head.map(_ => List.empty[Int])
    val xs = layers.foldLeft(zero) { (stack, layer) =>
      stack.zip(layer).map { case (s, pixel) => pixel :: s }
    }
    xs.map(_.reverse)
  }

  def computePixel(xs: List[Int]): Int = xs.iterator.find(_ != Transparent).get

  def renderImg(ps: List[Int], w: Int): String =
    ps.sliding(w, w)
      .map(x => x.map(x => if (x != White) " " else "■").mkString)
      .mkString("\n")


  def solutionPartB: String = {
    val img = stackLayers(layers).map(computePixel)
    renderImg(img, width)
  }
}

object Day8App extends App {
  println("SolutionPartA: " + Day8.solutionPartA)
  println("SolutionPartB:\n" + Day8.solutionPartB) // FPUAR
}
