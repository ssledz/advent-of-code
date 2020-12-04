package aof

import scala.annotation.tailrec

object Day04 extends Day with App {

  val day: String = "day04.txt"

  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val optional = Set("cid")
  val docs = documentScanner(lines)

  def documentScanner(xs: List[String]): List[List[(String, String)]] = {

    def keyValue(s: String) = {
      val arr = s.split(':')
      (arr(0), arr(1))
    }

    @tailrec
    def go(xs: List[String], current: List[(String, String)], acc: List[List[(String, String)]]): List[List[(String, String)]] = xs match {
      case h :: t if h.isBlank => go(t, List.empty, current :: acc)
      case h :: t =>
        val ys = h.split(' ').toList.map(keyValue)
        go(t, ys ::: current, acc)
      case Nil => if (current.isEmpty) acc else current :: acc
    }
    go(xs, List.empty, List.empty)
  }

  def solutionPartA: String = {
    val valid = docs.filter(doc => (doc.map(_._1).toSet -- optional) == required)
    valid.size.toString
  }

  def solutionPartB: String = {
    val valid = docs.filter { doc =>
      val req = doc.map(_._1).toSet -- optional == required
      if (req) {
        val m = doc.toMap
        val byr = m("byr").toInt
        val iyr = m("iyr").toInt
        val eyr = m("eyr").toInt
        val hgt = m("hgt")
        val hcl = m("hcl")
        val ecl = m("ecl")
        val pid = m("pid")

        val hgtValid = {
          val num = hgt.stripSuffix("cm").stripSuffix("in").toInt
          if (hgt.endsWith("cm")) num >= 150 && num <= 193 else num >= 59 && num <= 76
        }

        val byrValid = byr >= 1920 && byr <= 2002
        val iyrValid = iyr >= 2010 && iyr <= 2020
        val eyrValid = eyr >= 2020 && eyr <= 2030
        val hclValid = hcl.startsWith("#") && "[0-9a-f]{6}".r.matches(hcl.tail)
        val eclValid = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl)
        val pidValid = "[0-9]{9}".r.matches(pid)

//        println(s"""
//            |doc      : $doc
//            |byrValid : $byrValid
//            |iyrValid : $iyrValid
//            |eyrValid : $eyrValid
//            |hclValid : $hclValid
//            |eclValid : $eclValid
//            |pidValid : $pidValid
//            |""".stripMargin)

        byrValid &&
        iyrValid &&
        eyrValid &&
        hgtValid &&
        hclValid &&
        eclValid &&
        pidValid
      } else false
    }

    valid.size.toString
  }

  run()
}
