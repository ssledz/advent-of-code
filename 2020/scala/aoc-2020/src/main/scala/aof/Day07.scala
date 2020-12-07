package aof

import scala.annotation.tailrec

object Day07 extends Day with App {

  val day: String = "day07.txt"

  val bags = lines.map(Bag.from)

  val whereCanBePut = bags
    .flatMap(b => b.bags.keys.map(_ -> b.colour))
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2))
    .toMap

  def solutionPartA: String = {
    @tailrec
    def go(toVisit: List[String], visited: Set[String]): Set[String] = toVisit match {
      case h :: t =>
        val bags = whereCanBePut.getOrElse(h, List.empty)
        val notVisited = (t ++ bags).toSet.filterNot(visited.contains)
        go(notVisited.toList, visited + h)
      case Nil => visited
    }

    val bags = go(List("shiny gold"), Set.empty)

    (bags.size - 1).toString
  }

  def solutionPartB: String = {

    val mBags = bags.map(b => b.colour -> b).toMap

    def go(bag: String): Int = mBags.get(bag) match {
      case Some(b) =>
        1 + b.bags.map { case (bb, nn) => nn * go(bb) }.sum
      case None => 0
    }

    val cnt = go("shiny gold") - 1

    cnt.toString
  }

  case class Bag(colour: String, bags: Map[String, Int])

  object Bag {
    private val r = """(\d+) (.*) bags?.?$""".r
    private val label = "bags contain"
    def from(s: String): Bag = {
      val pos = s.indexOf(label)
      val colour = s.substring(0, pos).trim
      val arr = s.substring(pos + label.size).split(',').map(_.trim).toList
      val contains = arr.flatMap {
        case r(n, colour)     => List((colour, n.toInt))
        case "no other bags." => List.empty
      }
      Bag(colour, contains.toMap)
    }
  }

  run()
}
