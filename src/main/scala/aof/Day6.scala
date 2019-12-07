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

  def solutionPartB: String = ""
}

object Day6App extends App {

  println("SolutionPartA: " + Day6.solutionPartA)
  println("SolutionPartB: " + Day6.solutionPartB)

}