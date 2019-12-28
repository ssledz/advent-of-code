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
      case (chem, w) :: t => {
        if (chem == Ore) {
          val (all, _) = acc.get(chem).getOrElse((0, 0))
          go(t, acc.updated(chem, (all + w, 0)))
        } else {
          val r = reactions(chem)
          val m = r.input.foldLeft(acc) { case (mm, (chem, need)) =>
            val (all, left) = mm.get(chem).getOrElse((0, 0))
            if (need <= left) {
              mm.updated(chem, (all, left - need))
            } else {
              val rr = reactions(chem)
              val m2 = go(rr.input, mm)
              val todo = need - left - rr.w
              if (todo <= 0) {
                m2.updated(chem, (all + rr.w, todo.abs))
              } else {
                go(List((chem, todo)), m2.updated(chem, (all + rr.w, 0)))
              }
            }
          }
          val (all, left) = m.get(chem).getOrElse((0, 0))
          val todo = w - left - r.w
          if(todo <= 0) {
            m.updated(chem, (all + r.w, todo.abs))
          } else {
            go(List((chem, todo)), m.updated(chem, (all + r.w, 0)))
          }
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
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)

}
