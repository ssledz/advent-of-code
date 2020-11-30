package aof

object Day6 extends Day {
  val day: String = "06"

  def newDirectOrbits(xs: List[String]): Map[String, List[String]] = {
    val ys = xs.map(_.split(')').toList).map { case a :: b :: Nil => b -> a }
    ys.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  val directOrbits: Map[String, List[String]] = newDirectOrbits(lines)

  def aOrbits(all: Map[String, List[String]], a: String): List[String] = {
    all.get(a) match {
      case Some(directOrbits) => directOrbits ++ directOrbits.flatMap(aOrbits(all, _))
      case None => List.empty
    }
  }

  def countOrbits(all: Map[String, List[String]]): Int = {
    val xs = Map(all.keys.map(a => a -> aOrbits(all, a)).toSeq: _*)
    val cnt = xs.view.mapValues(_.length).toMap
    cnt.values.sum
  }

  def solutionPartA: String = {

    "" + countOrbits(directOrbits)
  }

  def orbitTransfers(all: Map[String, List[String]], a: String, b: String): List[String] = {

    val as = aOrbits(all, a).reverse
    val bs = aOrbits(all, b).reverse

    def go(as: List[String], bs: List[String], last: Option[String]): List[String] = (as.headOption, bs.headOption) match {
      case (Some(a), Some(b)) if a == b => go(as.tail, bs.tail, Some(a))
      case (Some(a), Some(b)) if a != b => as.reverse ++ last.toList ++ bs.reverse
      case (Some(_), None) => as.reverse ++ last.toList
      case (None, Some(_)) => last.toList ++ bs.reverse
      case (None, None) => List.empty
    }

    go(as, bs, None)

  }

  def solutionPartB: String = "" + (orbitTransfers(directOrbits, "YOU", "SAN").length - 1)
}

object Day6App extends App {

  println("SolutionPartA: " + Day6.solutionPartA)
  println("SolutionPartB: " + Day6.solutionPartB)

}