package aof

import aof.Day14.{solutionPartA, solutionPartB}

object Day14 extends Day {

  val day: String = "14"

  def toRections(lines: List[String]): Map[String, Reaction] = Map(lines.map { line =>
    val r = Reaction(line)
    (r.output, r)
  }: _*) + (Ore -> Reaction(1, Ore, List((Ore, 1))))

  lazy val reactions: Map[String, Reaction] = toRections(lines)

  def howManyOreToUse(reactions: Map[String, Reaction], r: String,
                      state: Map[String, (Long, Long)] = Map.empty): (Long, Map[String, (Long, Long)]) = {

    def go(xs: List[(String, Long)], acc: Map[String, (Long, Long)]): Map[String, (Long, Long)] = xs match {
      case (_, 0L) :: t => go(t, acc)
      case (Ore, w) :: t => {
        val (all, _) = acc.get(Ore).getOrElse((0L, 0L))
        go(t, acc.updated(Ore, (all + w, 0L)))
      }
      case (chem, need) :: t => {
        val (all, left) = acc.get(chem).getOrElse((0L, 0L))
        if (need <= left) {
          go(t, acc.updated(chem, (all, left - need)))
        } else {
          val r = reactions(chem)
          val todo = need - left - r.w
          go(r.input ::: List((chem, todo max 0L)) ::: t, acc.updated(chem, (all + r.w, (todo min 0L).abs)))
        }
      }
      case Nil => acc
    }

    val s = go(reactions(r).input, state)
    (s(Ore)._1, s)
  }

  def solutionPartA: String = {

    "" + howManyOreToUse(reactions, Fuel)._1
  }

  def produceFuel(reactions: Map[String, Reaction], ore: Long = 1_000_000_000_000L): Long = {

    def go(state: Map[String, (Long, Long)], fuel: Long, ore: Long): (Long, Map[String, (Long, Long)]) = {

      val (usedOre, newState) = howManyOreToUse(reactions, Fuel, state)

      println(s"${Math.round(usedOre * 100.0 / ore.toDouble)}% => " + fuel -> usedOre)

      if (usedOre > ore) {
        fuel -> state
      } else go(newState, fuel + 1, ore)
    }

    go(Map.empty, 0, ore)._1

  }

  def solutionPartB: String = ""

  val Ore = "ORE"

  val Fuel = "FUEL"

  case class Reaction(w: Long, output: String, input: List[(String, Long)])

  object Reaction {

    def toChem(s: String): (String, Long) = {
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
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
}
