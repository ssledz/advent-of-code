package aof

import aof.utils._

import scala.collection.mutable

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
    val mm = mutable.Map.empty[List[Set[String]], List[String]]
    def combine(xs: List[Set[String]]): List[String] = xs match {
      case h :: Nil => h.toList
      case h :: t =>
        h.flatMap { x =>
          memo(mm)(combine)(t).map(y => x + y)
        }.toList
      case Nil => List.empty
    }
    val m = mutable.Map.empty[Int, Set[String]]
    def expand(id: Int): Set[String] = {
      val rule = rules(id)
      rule match {
        case DependentRule(deps) =>
          deps.flatMap { rules: List[Int] =>
            val xs: List[Set[String]] = rules.map { rule =>
              memo(m)(expand)(rule)
            }
            memo(mm)(combine)(xs)
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
            memo(mm)(combine)(rs.map(memo(m)(expand)))
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

  def solutionPartB: String = {
    val expanded = expandRules(rules)

    //8: 42 | 42 8
    def matchRule8(s: String, acc: Option[String] = None): Option[String] = {
      val rule8 = expanded(8)
      def r(p: String) = s"""($p)+(.*)""".r
      val res = rule8.value.collectFirst {
        case rule if r(rule).matches(s) =>
          val pp = r(rule)
          s match {
            case pp(_, rest) => rest
          }
      }
      res match {
        case Some(value) => matchRule8(value, res)
        case None        => acc
      }
    }

    //11: 42 31 | 42 11 31
    def matchRule11(s: String, rule42: Set[String], rule31: Set[String]): Boolean = {
      def pairs(xs: Set[String], ys: Set[String]): List[(String, String)] =
        for {
          x <- xs.toList
          y <- ys
        } yield (x, y)
      def matches(a: String, b: String): Boolean = {
        def pattern(n: Int) = Seq.fill(n)(a).mkString + Seq.fill(n)(b).mkString
        (1 to 6).exists(n => pattern(n) == s)
      }
      val allPairs = pairs(rule42, rule31)
      def go(ss: String): Boolean =
        if (ss.isEmpty) true
        else {
          allPairs.find { case (pref, suff) => ss.startsWith(pref) && ss.endsWith(suff) } match {
            case Some((pref, suff)) => go(ss.stripPrefix(pref).stripSuffix(suff))
            case None               => false
          }
        }
      go(s)
    }

    // 0: 8 11
    def matches(s: String): Boolean =
      matchRule8(s) match {
        case Some(value) if !value.isBlank => matchRule11(value, expanded(42).value, expanded(31).value)
        case _                             => false
      }

    messages.count(matches).toString
  }

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
