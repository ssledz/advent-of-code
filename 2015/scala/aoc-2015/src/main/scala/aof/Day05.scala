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

  def niceString2(s: String): Boolean = {
    val cs = s.toCharArray.toList

    def go(xs: List[Char], maybeLast: Option[Char], i: Int, acc: List[((Char, Char), Int)]): List[((Char, Char), Int)] = {
      def overlapping(a: Char, b: Char, c: Char): Boolean = a.toString + b == b.toString + c

      (maybeLast, xs) match {
        case (None, a :: b :: c :: t) if overlapping(a, b, c) => go(c :: t, Some(b), i + 2, acc)
        case (None, a :: b :: t) => go(b :: t, Some(a), i + 1, ((a, b) -> i) :: acc)
        case (Some(last), a :: b :: c :: t) if !overlapping(last, a, b) && !overlapping(a, b, c) => go(b :: c :: t, Some(a), i + 1, ((a, b) -> i) :: acc)
        case (Some(last), a :: b :: Nil) if !overlapping(last, a, b) => ((a, b) -> i) :: acc
        case (Some(_), a :: t) => go(t, Some(a), i + 1, acc)
        case _ => acc
      }
    }

    val notOverlappingPairs = go(cs, None, 0, List.empty)

    val twoPairs = notOverlappingPairs.groupBy(_._1).view
      .mapValues(_.map(_._2).sorted)
      .filter(_._2.size >= 2)
      .mapValues(xs => (xs.min, xs.max)).toMap

    val ranges = twoPairs.values.toSet

    val b1 = !twoPairs.isEmpty

    def go2(xs: List[Char], i: Int, acc: List[(String, Int)]): List[(String, Int)] = {
      def pred(a: Char, b: Char, c: Char): Boolean = a == c

      def toStr(a: Char, b: Char, c: Char): String = a.toString + b + c

      xs match {
        case a :: b :: c :: t if pred(a, b, c) => go2(b :: c :: t, i + 1, (toStr(a, b, c) -> i) :: acc)
        case _ :: t => go2(t, i + 1, acc)
        case Nil => acc
      }
    }

    val repeatedLetter = go2(cs, 0, List.empty)

//    val b2 = repeatedLetter.exists { case (_, i) => ranges.exists { case (min, max) => i >= min && i <= max }}
    val b2 = !repeatedLetter.isEmpty

    b1 && b2
  }

  def solutionPartA: String = lines.map(niceString).count(identity).toString

  def solutionPartB: String = lines.map(niceString2).count(identity).toString

  run()

  println(niceString2("aaabbaa"))
//  println(lines.filter(niceString2).mkString("\n"))

}
