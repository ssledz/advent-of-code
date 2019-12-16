package aof

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

  def pairs[A](xs: Seq[A]): Seq[(A, A)] = {
    val ys = for {
      x <- xs
      y <- xs if x != y
    } yield (x, y)
    ys.foldLeft(Seq.empty[(A, A)]) { case (acc, p@(x, y)) =>
      if (acc contains(y, x)) {
        acc
      } else acc :+ p
    }
  }

  def applyGravity(xs: Seq[Moon]): Seq[Moon] = {

    val ys = pairs(xs).map { case (a, b) =>
      val (dva, dvb) = gravityChange(a, b)
      List(a -> dva, b -> dvb)
    }

    val yys = ys.flatten.groupBy(_._1).toList.map { case (moon, zs) =>
      val dv = zs.map(_._2).foldLeft(Vector3d(0, 0, 0))(_ + _)
      moon.copy(velocity = moon.velocity + dv)
    }
    yys
  }

  def applyVelocity(xs: Seq[Moon]): Seq[Moon] =
    xs.map { moon =>
      moon.copy(pos = moon.pos + moon.velocity)
    }

  def step(xs: Seq[Moon]): Seq[Moon] = {
    val f = applyGravity _ andThen applyVelocity
    f(xs)
  }

  def steps(xs: Seq[Moon], n: Int): Seq[Moon] =
    (1 to n).foldLeft(xs)((ms, _) => step(ms).toList)

  def totalEnergy(xs: Seq[Moon]): Int =
    xs.foldLeft(0)((acc, m) => acc + m.totalEnergy)

  def solutionPartA: String = {
    val xs = steps(moons, 1000)
    "" + totalEnergy(xs)
  }

  def solutionPartB: String = ""

  case class Vector3d(x: Int, y: Int, z: Int) {
    def +(other: Vector3d): Vector3d = Vector3d(x + other.x, y + other.y, z + other.z)
  }

  case class Moon(pos: Vector3d, velocity: Vector3d = Vector3d(0, 0, 0)) {
    def pot: Int = Math.abs(pos.x) + Math.abs(pos.y) + Math.abs(pos.z)

    def kin: Int = Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)

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
  println("SolutionPartA: " + Day12.solutionPartA)
  println("SolutionPartB: " + Day12.solutionPartB)
}
