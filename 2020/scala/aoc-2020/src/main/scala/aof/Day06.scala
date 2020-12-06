package aof

import scala.annotation.tailrec

object Day06 extends Day with App {

  val day: String = "day06.txt"

  def parseGroups(lines: List[String]): List[Group] = {
    @tailrec
    def go(xs: List[String], curr: Group, acc: List[Group]): List[Group] = xs match {
      case h :: t if h.isBlank       => go(t, Group(List.empty), curr :: acc)
      case h :: t                    => go(t, Group(h.toCharArray.toList :: curr.forms), acc)
      case Nil if curr.forms.isEmpty => acc
      case Nil                       => curr :: acc
    }
    go(lines, Group(List.empty), List.empty)
  }

  val groups = parseGroups(lines)

  def solutionPartA: String = {
    val xs = groups.map { group =>
      group.forms.flatten.toSet.size
    }
    xs.sum.toString
  }

  def solutionPartB: String = {
    val xs = groups.map { group =>
      val n = group.forms.size
      val ys = group.forms.flatMap(qs => qs.map(q => (q, 1))).groupBy(_._1).view.mapValues(_.size)
      val intersection = ys.filter { case (_, v) => v == n }
      intersection.keys.size
    }
    xs.sum.toString
  }

  run()

  case class Group(forms: List[List[Char]])
}
