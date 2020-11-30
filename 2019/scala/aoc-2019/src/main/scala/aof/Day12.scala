package aof

/**
 *
 * https://en.wikipedia.org/wiki/Least_common_multiple
 *
 * Planetary alignment
 * Suppose there are three planets revolving around a star which take l, m and n units of time respectively to
 * complete their orbits. Assume that l, m and n are integers. Assuming the planets started moving around the star
 * after an initial linear alignment, all the planets attain a linear alignment again after LCM(l, m, n) units of time.
 * At this time, the first, second and third planet will have completed LCM(l, m, n)/l, LCM(l, m, n)/m and LCM(l, m, n)/n
 * orbits respectively around the star.[4]
 *
 */
object Day12 extends Day {

  val day: String = "12"

  lazy val moons = lines.map(Moon.from)

  val Zero = Vector3d(0, 0, 0)

  def g(a: Long, b: Long): Int = if (a < b) 1 else if (a > b) -1 else 0

  def gc(a: Moon, b: Moon): Vector3d = Vector3d(g(a.pos.x, b.pos.x), g(a.pos.y, b.pos.y), g(a.pos.z, b.pos.z))

  def step(xs: Seq[Moon]): Seq[Moon] = {
    val dv = xs.map(x => xs.filter(_ != x).map(y => gc(x, y)).foldLeft(Zero)(_ + _))
    xs.zip(dv).map { case (moon, d) =>
      val v = moon.velocity + d
      moon.copy(pos = moon.pos + v, velocity = v)
    }
  }

  def steps(xs: Seq[Moon], n: Int): Seq[Moon] =
    (1 to n).foldLeft(xs)((ms, _) => step(ms).toList)

  def totalEnergy(xs: Seq[Moon]): Long = xs.foldLeft(0L)((acc, m) => acc + m.totalEnergy)

  def solutionPartA: String = "" + totalEnergy(steps(moons, 1000))

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  def lcm(a: BigInt, b: BigInt): BigInt = (a * b) / gcd(a, b)

  val n = 3

  def reachInitialState(xs: Seq[Moon]): BigInt = {

    def go(ms: Seq[Moon], iter: Long = 0, state: Seq[Set[Seq[Int]]] = Seq.fill(n)(Set.empty), acc: Seq[Long] = Seq.fill(n)(0)): Seq[Long] = {
      if (acc.forall(_ != 0)) {
        acc
      } else {

        val ns = step(ms)

        val ys = acc.zipWithIndex.zip(state).map { case ((n, i), s) =>

          if (n == 0) {
            val xs = ns.map(x => x.pos.toVec(i)) ++ ns.map(x => x.velocity.toVec(i))
            if (s.contains(xs)) (iter, s + xs) else (n, s + xs)
          } else (n, s)

        }

        go(ns, iter + 1, ys.map(_._2), ys.map(_._1))

      }

    }


    go(xs).map(BigInt.apply).iterator.reduce(lcm)

  }

  def solutionPartB: String = "" + reachInitialState(moons)


  case class Vector3d(x: Int, y: Int, z: Int) {
    def +(other: Vector3d): Vector3d = Vector3d(x + other.x, y + other.y, z + other.z)

    def toVec: Vector[Int] = Vector(x, y, z)
  }

  case class Moon(pos: Vector3d, velocity: Vector3d = Vector3d(0, 0, 0)) {

    def pot: Long = pos.x.abs + pos.y.abs + pos.z.abs

    def kin: Long = velocity.x.abs + velocity.y.abs + velocity.z.abs

    def totalEnergy: Long = pot * kin
  }

  object Moon {

    val MoonRegex = raw"<x=([^,]*), y=([^,]*), z=([^,]*)>".r

    def from(s: String): Moon = s match {
      case MoonRegex(x, y, z) => Moon(Vector3d(x.toInt, y.toInt, z.toInt))
    }
  }

}

object Day12App extends App {
  println("SolutionPartA: " + Day12.solutionPartA)
  println("SolutionPartB: " + Day12.solutionPartB)
}
