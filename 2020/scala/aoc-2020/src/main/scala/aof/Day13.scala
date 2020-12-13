package aof

object Day13 extends Day with App {

  type Id = Long

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

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

  def timestampOf(xs: List[(Id, Int)]): Long = {
    val lc = xs.map(_._1).reduce(lcm)
    val ys: List[(Long, Id, Int)] = xs.map { case (id, i) => (lc / id, id, i) }

    def iter(t: Long, xs: List[(Long, Id, Int)], dt: Long): Long = {
      val stop = xs.forall { case (l, id, i) => l * (t + i) % id == 0 }
      if (stop) t else iter(t + dt, xs, dt)
    }
    val (_, dt, _) = ys.head
//    val dt2 = ys.filter(_._3 % dt == 0).map(x => x._2 - x._3).max
    iter(0, ys, dt)
  }

  def solutionPartB: String = {
    val xs = ids.zipWithIndex.filterNot(_._1 == "x").map { case (x, i) => x.toLong -> i }
    timestampOf(xs).toString
  }

  run()
}
