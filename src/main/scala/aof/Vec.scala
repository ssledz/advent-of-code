package aof

case class Vec(x: Int, y: Int) {
  def +(other: Vec): Vec = Vec(x + other.x, y + other.y)

  def min(other: Vec): Vec = Vec(x min other.x, y min other.y)

  def max(other: Vec): Vec = Vec(x max other.x, y max other.y)

  def manhattanLen: Int = x + y
}

object Vec {
  val MaxValue = Vec(Int.MaxValue, Int.MaxValue)
  val MinValue = Vec(Int.MinValue, Int.MinValue)
  val Zero = Vec(0, 0)
}
