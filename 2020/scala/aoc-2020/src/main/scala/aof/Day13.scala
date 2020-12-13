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

  def timestampOfSlow(xs: List[(Id, Int)], t: Long = 0, maybeDt: Option[Long] = None): Long = {
    val lc = xs.map(_._1).reduce(lcm)
    val ys: List[(Long, Id, Int)] = xs.map { case (id, i) => (lc / id, id, i) }

    def iter(t: Long, xs: List[(Long, Id, Int)], dt: Long): Long = {
      val stop = xs.forall { case (l, id, i) => (t + i) % id == 0 }
      if (stop) t else iter(t + dt, xs, dt)
    }
    val (_, dt, _) = ys.head
    iter((t / dt) * dt, ys, maybeDt.getOrElse(dt))
  }

  def timestampOfFast(xs: List[(Id, Int)], t: Long = 0, maybeDt: Option[Long] = None): Long = {
    val lc = xs.map(_._1).reduce(lcm)
    val ys: List[(Long, Id, Int)] = xs.map { case (id, i) => (lc / id, id, i) }
    val (_, dt, _) = ys.head
    val (rm, dt2) = {
      val zz = ys.filter(_._3 % dt == 0).map(x => x._2)
      (if (zz.size == 1) 0L else dt) -> zz.foldLeft(dt)(lcm)
    }

    def iter(t: Long, xs: List[(Long, Id, Int)], dt: Long): Long = {
      val stop = xs.forall { case (l, id, i) => (t + i - rm) % id == 0 }
      if (stop) t else iter(t + dt, xs, dt)
    }

    iter((t / dt2) * dt2, ys, maybeDt.getOrElse(dt2)) - rm
  }

  def solutionPartB: String = {
    val xs = ids.zipWithIndex.filterNot(_._1 == "x").map { case (x, i) => x.toLong -> i }
//    timestampOfFast(xs, 760171380521445L, None).toString
    timestampOfFast(xs, 760171380521445L, None).toString

//    val dt = lcm(17, 643)
//    println(dt)
//    val lc = xs.map(_._1).reduce(lcm)
//    val ys: List[(Long, Id, Int)] = xs.map { case (id, i) => (lc / id, id, i) }
//
//    def iter(t: Long, dt: Long, tt: Long): Long = {
//      if (tt < t) {
//        println(t)
//      }
//      val stop = ys.forall { case (l, id, i) =>
//        val tmp = (t + i - 17)
//        tmp % id == 0
//      }
//      if (stop) t else iter(t + dt, dt, if (tt < t) t + 100000000 * dt else tt)
//    }
//
//    val start = 760171380521445L
//    val xxx = iter((start / dt) * dt, dt, 0) - 17
//    println(xxx)

  }

  run()
}
