package aof

import aof.Day12.{solutionPartA, solutionPartB}

object Day12 extends Day {

  val day: String = "12"

  lazy val moons = lines.map(Moon.from)

  def gravityChange(a: Int, b: Int): (Int, Int) =
    if (a < b) (1, -1) else if (a > b) (-1, 1) else (0, 0)

  def gravityChange(a: Moon, b: Moon): (Vector3d, Vector3d) = {
    val (xa, xb) = gravityChange(a.pos.x, b.pos.x)
    val (ya, yb) = gravityChange(a.pos.y, b.pos.y)
    val (za, zb) = gravityChange(a.pos.z, b.pos.z)
    (Vector3d(xa, ya, za), Vector3d(xb, yb, zb))
  }

  def applyGravity(xs: Seq[Moon]): Seq[Moon] = {
    val ys = for {
      a <- xs
      b <- xs if a != b
    } yield {
      val (dva, dvb) = gravityChange(a, b)
      List(a -> dva, b -> dvb)
    }

    ys.flatten.groupBy(_._1).toList.map { case (moon, zs) =>
      val dv = zs.map(_._2).foldLeft(Vector3d(0, 0, 0))(_ + _)
      moon.copy(velocity = moon.velocity + dv)
    }
  }

  def applyVelocity(xs: Seq[Moon]): Seq[Moon] =
    xs.map { moon =>
      moon.copy(pos = moon.pos + moon.velocity)
    }

  def step(xs: Seq[Moon]): Seq[Moon] = {
    val f = applyGravity _ andThen applyVelocity
    f(xs)
  }

  def solutionPartA: String = {
    val xs = (1 to 1000).foldLeft(moons)((ms, _) => step(ms).toList)
    "" + xs.foldLeft(0)((acc, m) => acc + m.totalEnergy)
  }

  def solutionPartB: String = ""

  case class Vector3d(x: Int, y: Int, z: Int) {
    def +(other: Vector3d): Vector3d = Vector3d(x + other.x, y + other.y, z + other.z)
  }

  case class Moon(pos: Vector3d, velocity: Vector3d = Vector3d(0, 0, 0)) {
    def pot: Int = pos.x + pos.y + pos.z

    def kin: Int = velocity.x + velocity.y + velocity.z

    def totalEnergy: Int = pot * kin
  }

  object Moon {

    val MoonRegex = raw"<x=([^,]*), y=([^,]*), z=([^,]*)>".r

    def from(s: String): Moon = s match {
      case MoonRegex(x, y, z) => Moon(Vector3d(x.toInt, y.toInt, z.toInt))
    }
  }

}

object Day12App extends App {
  // You guessed 96.
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
