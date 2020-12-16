package aof

import scala.annotation.tailrec

object Day16 extends Day with App {

  val day: String = "day16.txt"

  @tailrec
  def readFields(xs: List[String], acc: List[Field]): (List[String], List[Field]) = xs match {
    case h :: t if h.isBlank => (t, acc)
    case h :: t              => readFields(t, Field.from(h) :: acc)
  }

  def readInput(xs: List[String]): (List[Field], List[Int], List[List[Int]]) = {
    val (ys, fields) = readFields(xs, List.empty)
    assert(ys.head == "your ticket:")
    val tickets = ys.tail.head.split(',').map(_.toInt).toList
    val zs = ys.tail.tail.tail
    assert(zs.head == "nearby tickets:")
    (fields, tickets, zs.tail.map(_.split(',').map(_.toInt).toList))
  }

  val (fields, myTickets, nearbyTickets) = readInput(lines)

  def solutionPartA: String = {
    def go(xs: List[List[Int]], err: List[Int]): List[Int] = xs match {
      case ys :: t =>
        go(t, ys.filter(y => !fields.exists(f => f.isValid(y))) ::: err)
      case Nil => err
    }

    go(nearbyTickets, List.empty).sum.toString
  }

  def solutionPartB: String =
    ""

  run()

  case class Field(name: String, rl: Range, rr: Range) {
    def isValid(x: Int): Boolean = rl.contains(x) || rr.contains(x)
  }

  object Field {

    private val ranges = """(\d+)-(\d+) or (\d+)-(\d+)""".r

    def from(s: String): Field = {
      val arr = s.split(':')
      arr(1).trim match {
        case ranges(a, b, c, d) => Field(arr(0).trim, a.toInt to b.toInt, c.toInt to d.toInt)
      }
    }
  }

}
