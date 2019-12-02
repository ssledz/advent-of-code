package aof

object Day1 extends Day with App {

  val day = "01"

  def fuel(mass: Int): Int = mass / 3 - 2

  val res1 = lines.map(_.toInt).foldLeft(0)((acc, m) => acc + fuel(m))

  println("checks:")
  List(12, 14, 1969, 100756).foreach { m =>
    println(s"mass : $m => " + fuel(m))
  }
  println("part1: " + res1)

  def streamOfFuel(m: Int): List[Int] = {
    val x = fuel(m)
    if (x <= 0) {
      List.empty
    } else {
      x :: streamOfFuel(x)
    }
  }

  println("checks:")

  List(12, 14, 1969, 100756).foreach { m =>
    val x = streamOfFuel(m)
    println(s"mass : $m => $x (" + x.sum + ")")
  }

  val res2 = lines.map(_.toInt).flatMap(streamOfFuel).sum

  println("part2: " + res2)
}
