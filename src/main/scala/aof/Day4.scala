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

  def passwords(r: Range, check: String => Boolean): List[String] = r.toList.map(_.toString).filter(check)

  def additionalPassCheck(p: String): Boolean = {

    def go(xs: List[Char], current: Char, cnt: Int, acc: Boolean): Boolean = xs match {
      case h :: t => {
        if (h == current) go(t, h, cnt + 1, acc)
        else go(t, h, 1, acc || cnt == 2)
      }
      case Nil => acc || cnt == 2
    }

    val xs = p.toList
    go(xs.tail, xs.head, 1, false)
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

  def printFullPassCheck(s: String): Unit = println(s"$s full passes check: " + fullPassCheck(s))

  printFullPassCheck("112233")
  printFullPassCheck("123444")
  printFullPassCheck("111122")
  printFullPassCheck("122345")
  printFullPassCheck("122245")
  printFullPassCheck("111111")
  printFullPassCheck("111122")
  printFullPassCheck("111223")

  println(s"number of passwords with fullPassCheck in range $range: " + passwords(range, fullPassCheck).length)

}


