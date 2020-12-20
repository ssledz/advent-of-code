package aof

object Day19 extends Day with App {

  val day: String = "day19.txt"

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
    def combine(xs: List[Set[String]]): List[String] = xs match {
      case h :: Nil => h.toList
      case h :: t =>
        h.flatMap { x =>
          combine(t).map(y => x + y)
        }.toList
      case Nil => List.empty
    }
    def expand(id: Int): Set[String] = {
      val rule = rules(id)
      rule match {
        case DependentRule(deps) =>
          deps.flatMap { rules: List[Int] =>
            val xs: List[Set[String]] = rules.map { rule =>
              expand(rule)
            }
            combine(xs)
          }.toSet
        case ExpandedRule(value) => value
      }
    }
    val depRules = rules.collect { case (id, r: DependentRule) => (id, r) }.toList
    val expanded = rules.collect { case (id, r: ExpandedRule)  => (id, r) }.toList
    depRules
      .foldLeft(expanded) {
        case (acc, (id, rule)) =>
          val value = rule.deps.flatMap { rs =>
            combine(rs.map(expand))
          }
          (id, ExpandedRule(value.toSet)) :: acc
      }
      .toMap
  }

  def solutionPartA: String = {
    val expanded = expandRules(rules)
    val zeroRule = expanded(0)
    messages.count(zeroRule.value.contains).toString
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
