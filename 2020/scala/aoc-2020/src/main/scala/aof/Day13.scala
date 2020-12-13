package aof

object Day13 extends Day with App {

  val day: String = "day13.txt"

  val ts = lines.head.toInt

  val ids = lines.tail.head.split(',').toList

  def wait(ts: Int, id: Int): Int = {
    val x = (ts / id) * id
    if (x == ts) 0 else x + id - ts
  }

  def solutionPartA: String = {
    val xs = ids.filterNot(_ == "x").map(_.toInt).map(id => (id, wait(ts, id)))
    val (id, dt) = xs.minBy(_._2)
    (id * dt).toString
  }

  def solutionPartB: String =
    ""

  run()
}
