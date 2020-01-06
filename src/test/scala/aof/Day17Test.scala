package aof

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  test("asciiEncode") {
    assert(Day17.asciiEncode(Seq("A", "B", "C", "B", "A", "C")) == Seq(65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67))
    assert(Day17.asciiEncode(Seq("R", "8", "R", "8")) == Seq(82, 44, 56, 44, 82, 44, 56))
    assert(Day17.asciiEncode(Seq("R", "4", "R", "4", "R", "8")) == Seq(82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56))
    assert(Day17.asciiEncode(Seq("L", "6", "L", "2")) == Seq(76, 44, 54, 44, 76, 44, 50))
    assert(Day17.asciiEncode(Seq("L")) == Seq(76))
    assert(Day17.asciiEncode(Seq("L", "84")) == Seq(76, 44, 56, 52))
  }

  test("encodeRoutines") {
    val xs = Day17.encodeRoutines(Day17Test.Ex1).map {
      case (routine, (a, b, c)) => (routine, (a.mkString(","), b.mkString(","), c.mkString(",")))
    }

    val zs = xs.map { case (routine, (aFun, bFun, cFun)) =>
      routine + " =>\n  A=[" + aFun + "]" + "\n  B=[" + bFun + "]" + "\n  C=[" + cFun + "]"
    }.mkString("\n\n")

    println(zs)

    assert(xs contains("A,B,C,B,A,C", ("R,8,R,8", "R,4,R,4,R,8", "L,6,L,2")))
  }

  test("encodeFunction") {

    val xs: Set[(Seq[String], String)] = Day17.encodeFunction("A,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,A,L,6,L,2".split(',').toList, "B")

    val ys = xs.map(x => x._1.mkString(",") + " => " + x._2).mkString("\n")
    println(ys)

  }


}

object Day17Test {

  val Ex1 = List("R", "8", "R", "8", "R", "4", "R", "4", "R", "8", "L", "6", "L", "2", "R", "4", "R", "4", "R", "8", "R", "8", "R", "8", "L", "6", "L", "2")

}
