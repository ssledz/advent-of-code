package aof

object Day19 extends Day with App {

  val day: String = "day20.txt"

  def parse: (Map[Int, Rule], List[String]) = {
    def go(xs: List[String], rules: List[(Int, Rule)]): (Map[Int, Rule], List[String]) = xs match {
      case h :: t if h.isBlank => (rules.toMap, t)
      case h :: t              => go(t, Rule.from(h) :: rules)
    }
    go(lines, List.empty)
  }

  val (rules, messages) = parse

  rules.values.toList

  def expandRules(rules: Map[Int, Rule]): Map[Int, ExpandedRule] = {

    def go(xs: List[DependentRule], acc: Map[Int, ExpandedRule]): Map[Int, ExpandedRule] = xs match {
      case h :: t =>
        ???
      case Nil => acc
    }

    val depRules = rules.values.collect { case r: DependentRule => r }.toList
    val expanded = rules.collect { case (id, r: ExpandedRule) => (id, r) }
    go(depRules, expanded)
  }

  def solutionPartA: String = {
    println(rules)
    println
    println(messages)
    ""
  }

  def solutionPartB: String =
    ""

  run()

  sealed trait Rule

  object Rule {
    def from(s: String): (Int, Rule) = {
      val arr = s.split(':')
      if (arr(1).trim.startsWith("\"")) {
        arr(0).toInt -> ExpandedRule(Set(arr(1).trim.tail.init))
      } else {
        val deps = arr(1).trim.split('|').map(_.trim.split(' ').map(_.toInt).toList).toList
        arr(0).toInt -> DependentRule(deps)
      }
    }
  }

  case class DependentRule(deps: List[List[Int]]) extends Rule
  case class ExpandedRule(value: Set[String]) extends Rule
}
