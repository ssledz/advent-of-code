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
    def enc(s: String) = Day17.encodeRoutines(s.split(',')).map {
      case (routine, (a, b, c)) => (routine, (a.mkString(","), b.mkString(","), c.mkString(",")))
    }

//    assert(enc(Day17Test.Ex1) contains("A,B,C,B,A,C", ("R,8,R,8", "R,4,R,4,R,8", "L,6,L,2")))

//    println(Day17.encodeRoutines(Day17Test.Ex2.split(',')))

//    println("dupa: " + enc(Day17Test.Ex2))

    def dbg(x: (Seq[String], String), label: String): String = s"$label: [${x._1.mkString(",")}] => " + x._2

    Day17.encodeFunction(Day17Test.Ex2.split(','), "A").foreach { case aF@(_, routineA) =>

      Day17.encodeFunction(routineA.split(','), "B").foreach { case bF@(_, routineB) =>

        Day17.encodeFunction(routineB.split(','), "C").foreach { case cF@(_, routineC) =>

          val ws = routineC.split(',').toSet.removedAll(Set("A", "B", "C"))

          if (ws.isEmpty) {
            println()
            println(Day17Test.Ex2)
            println(dbg(aF, "A"))
            println(dbg(bF, "B"))
            println(dbg(cF, "C"))
            println("ws: " + ws)
          }

        }


      }

    }

    //    println("xs: " + enc(Day17Test.Ex2))

  }

  test("encodeFunction") {

    val xs: Set[(Seq[String], String)] = Day17.encodeFunction("A,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,A,L,6,L,2".split(',').toList, "B")

    val ys = xs.map(x => (x._1.mkString(","), x._2))

    assert(ys contains("R,4,R,4,R,8", "A,B,L,6,L,2,B,A,L,6,L,2"))
    assert(ys contains("R,4,R,4,R", "A,B,8,L,6,L,2,B,8,A,L,6,L,2"))
    assert(ys contains("R,4,R,4", "A,B,R,8,L,6,L,2,B,R,8,A,L,6,L,2"))
    assert(ys contains("R,4,R", "A,B,4,R,8,L,6,L,2,R,4,B,8,A,L,6,L,2"))
    assert(ys contains("R,4", "A,B,B,R,8,L,6,L,2,B,B,R,8,A,L,6,L,2"))
    assert(ys contains("R", "A,B,4,B,4,B,8,L,6,L,2,B,4,B,4,B,8,A,L,6,L,2"))

  }

  test("replace") {
    val xs = Day17.replace("R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2", "R,8,R,8", "A")
    assert(xs contains "A,R,4,R,4,R,8,L,6,L,2,R,4,R,4,A,R,8,L,6,L,2")
    assert(xs contains "A,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,A,L,6,L,2")
    val ys = Day17.replace("R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2", "R,8,R,8,R", "A")
    assert(ys contains "A,4,R,4,R,8,L,6,L,2,R,4,R,4,A,8,L,6,L,2")
    val zs = Day17.replace("R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2", "R,8,R,8,R,8", "A")
    assert(zs contains "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,A,L,6,L,2")
  }


}

object Day17Test {
  val Ex1 = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
  val Ex2 = "L,4,L,4,L,10,R,4,R,4,L,4,L,4,R,8,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10,R,4,L,10,R,10,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10"
}
