package aof

object Day05 extends Day with App {

  val day: String = "day05.txt"

  val vowels = Set('a', 'e', 'i', 'o', 'u')

  val forbidden = Set("ab", "cd", "pq", "xy")

  def niceString(s: String): Boolean = {
    val arr = s.toCharArray
    val vc = arr.filter(c => vowels.contains(c)).size
    lazy val tc = arr.zip(arr.drop(1)).filter { case (a, b) => a == b }.size
    lazy val nc = forbidden.forall(x => !s.contains(x))
    (vc >= 3) && (tc >= 1) && nc
  }

  def nicerString2(s: String): Boolean = {
    val r1 = """(..).*\1""".r
    val r2 = """(.).\1""".r
    r1.findFirstIn(s).isDefined && r2.findFirstIn(s).isDefined
  }

  def nicerString(s: String): Boolean = {
    val cs = s.toList

    def go(xs: List[Char]): Boolean = xs match {
      case a :: b :: t if t.mkString.contains(a.toString + b) => true
      case _ :: t => go(t)
      case Nil => false
    }

    val b1 = go(cs)
    val b2 = cs.zip(cs.drop(1)).zip(cs.drop(2)).exists { case ((a, _), c) => a == c }
    b1 && b2
  }

  def solutionPartA: String = lines.map(niceString).count(identity).toString

  def solutionPartB: String = lines.map(nicerString).count(identity).toString

  run()

}
