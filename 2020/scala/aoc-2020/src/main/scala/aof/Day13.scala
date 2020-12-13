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

  def timestampOf(xs: List[(Id, Int)], t: Long = 0, maybeDt: Option[Long] = None): Long = {
    val ys: List[(Id, Int)] = xs.map { case (id, i) => (id, i) }
    val (dt, _) = ys.head
    val (rm, dt2) = {
      val zz = ys.filter(_._2 % dt == 0).map(x => x._1)
      (if (zz.size == 1) 0L else dt) -> zz.foldLeft(dt)(lcm)
    }

    def iter(t: Long, xs: List[(Id, Int)], dt: Long, tt: Long): Long = {
      if (tt < t) {
        println("dbg: " + t)
      }
      val stop = xs.forall { case (id, i) => (t + i - rm) % id == 0 }
      if (stop) t else iter(t + dt, xs, dt, if (tt < t) t + 100000000 * dt else tt)
    }

    iter((t / dt2) * dt2, ys, maybeDt.getOrElse(dt2), 0) - rm
  }

  def solutionPartB: String = {
    val xs = ids.zipWithIndex.filterNot(_._1 == "x").map { case (x, i) => x.toLong -> i }
    timestampOf(xs, 760171380521445L, None).toString
  }

  run()
}
