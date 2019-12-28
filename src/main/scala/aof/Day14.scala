package aof

import aof.Day14.{solutionPartA, solutionPartB}

object Day14 extends Day {

  val day: String = "14"

  def toRections(lines: List[String]): Map[String, Reaction] = Map(lines.map { line =>
    val r = Reaction(line)
    (r.output, r)
  }: _*) + (Ore -> Reaction(1, Ore, List((Ore, 1))))

  lazy val reactions: Map[String, Reaction] = toRections(lines)

  def howManyOreToUse(reactions: Map[String, Reaction], r: String): Int = {

    def go(xs: List[(String, Int)], acc: Map[String, (Int, Int)]): Map[String, (Int, Int)] = xs match {
      case (_, 0) :: t => go(t, acc)
      case (Ore, w) :: t => {
        val (all, _) = acc.get(Ore).getOrElse((0, 0))
        go(t, acc.updated(Ore, (all + w, 0)))
      }
      case (chem, need) :: t => {
        val (all, left) = acc.get(chem).getOrElse((0, 0))
        if (need <= left) {
          go(t, acc.updated(chem, (all, left - need)))
        } else {
          val r = reactions(chem)
          val todo = need - left - r.w
          go(r.input ::: List((chem, todo max 0)) ::: t, acc.updated(chem, (all + r.w, (todo min 0).abs)))
        }
      }
      case Nil => acc
    }

    go(reactions(r).input, Map.empty)(Ore)._1
  }

  def solutionPartA: String = {

    "" + howManyOreToUse(reactions, Fuel)
  }

  def solutionPartB: String = ""

  val Ore = "ORE"

  val Fuel = "FUEL"

  case class Reaction(w: Int, output: String, input: List[(String, Int)])

  object Reaction {

    def toChem(s: String): (String, Int) = {
      s.trim.split(' ').toList match {
        case w :: chem :: Nil => (chem, w.toInt)
      }
    }

    def apply(s: String): Reaction = {
      val l :: r :: Nil = s.split("=>").map(_.trim).toList
      val ll = l.split(',').toList.map(toChem)
      val (output, w) = toChem(r)
      Reaction(w, output, ll)
    }

  }

}

object Day14App extends App {
  //Your answer is too low. You guessed 66360.
  // 486641
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)

}
