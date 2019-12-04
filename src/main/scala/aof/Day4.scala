package aof

object Day4 extends Day with App {

  override val day = "04"

  def passCheck(p: String): Boolean = {

    def lenCheck: Boolean = p.length == 6

    def adjacentTheSame: Boolean = {
      val xs = p.toList
      xs.zip(xs.tail).exists { case (l, r) => l == r }
    }

    def neverDecrease: Boolean = {
      val xs = p.toList
      xs.zip(xs.tail).forall { case (l, r) => l.toInt <= r.toInt }
    }

    lenCheck && adjacentTheSame && neverDecrease

  }

  def passwords(r: Range, check : String => Boolean): List[String] = r.toList.map(_.toString).filter(check)

  def additionalPassCheck(p: String): Boolean = {
    val xs = p.toList
    val ys = xs.zip(xs.tail).zip(xs.tail.tail)

    ys.foldLeft[(Boolean, Option[Char])]((false, None)) { case ((found, acc), ((a, b), c)) =>
      if (found) {
        (found, None)
      } else acc match {
        case Some(aa) => if (aa == a) (false, None) else (true, None)
        case None => if (a == b && b != c) (true, None) else if (b == c && a != b) (false, Some(b)) else (found, None)
      }
    }._1

  }

  def fullPassCheck(p: String): Boolean = passCheck(p) && additionalPassCheck(p)

  println("111111 pass check: " + passCheck("111111"))
  println("223450 pass check: " + passCheck("223450"))
  println("123789 pass check: " + passCheck("123789"))

  val l :: r :: Nil = lines.head.split('-').toList

  println(l + " - " + r)

  val range = l.toInt to r.toInt

  //1748
  println(s"number of passwords with passCheck in range $range: " + passwords(range, passCheck).length)

  println("112233 full passes check: " + fullPassCheck("112233"))
  println("123444 full passes check: " + fullPassCheck("123444"))
  println("111122 full passes check: " + fullPassCheck("111122"))

  println(s"number of passwords with fullPassCheck in range $range: " + passwords(range, fullPassCheck).length)
}


