package aof

object Day8 extends Day {

  val day: String = "08"

  val image = lines.flatMap(_.toCharArray)

  val width: Int = 25
  val tall: Int = 6

  def toLayers(img: List[Char], w: Int, h: Int): List[List[Char]] = {

    def go(i: Int, acc: List[List[Char]]): List[List[Char]] = {
      if (img.length == i) {
        acc
      } else {
        go(i + w * h, img.slice(i, i + w * h) :: acc)
      }
    }

    go(0, List.empty)
  }

  val frames: List[List[Char]] = toLayers(image, width, tall)

  def solutionPartA: String = {

    val (_, x) = frames.map(xs => (xs.count(_ == '0'), xs)).sortBy(_._1).head

    val ret = x.count(_ == '1') * x.count(_ == '2')

    "" + ret
  }

  def solutionPartB: String = ""
}

object Day8App extends App {
  println("SolutionPartA: " + Day8.solutionPartA)
  println("SolutionPartB: " + Day8.solutionPartB)
}
