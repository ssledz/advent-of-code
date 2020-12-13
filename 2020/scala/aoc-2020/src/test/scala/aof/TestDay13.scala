package aof

import aof.Day13.Id
import aof.TestDay13.Examples
import org.scalatest.funsuite.AnyFunSuite

class TestDay13 extends AnyFunSuite {

  test("test timestampOf") {
    testAlg(Day13.timestampOf)
  }

  test("test") {
    //3417  =>  List((17,0), (13,2), (19,3))
    //102(17)   => List((17,0), (13,2))
    //208(13)  =>  List((13,0), (19,1))
    println(Day13.timestampOf(List((17L, 0), (13L, 2), (19L, 3))))
    println(Day13.timestampOf(List((17L, 0), (13L, 2))))
    println(Day13.timestampOf(List((13L, 0), (19L, 1))))
  }

  def testAlg(f: List[(Id, Int)] => Long): Unit =
    Examples.foreach { case (xs, ts) => assert(f(xs) === ts) }

}

object TestDay13 {
  val Examples = List(
    List((7L, 0), (13L, 1), (59L, 4), (31L, 6), (19L, 7)) -> 1068781,
    List((17L, 0), (13L, 2), (19L, 3)) -> 3417,
    List((1789L, 0), (37L, 1), (47L, 2), (1889L, 3)) -> 1202161486,
    List((67L, 0), (7L, 1), (59L, 2), (61L, 3)) -> 754018,
    List((67L, 0), (7L, 2), (59L, 3), (61L, 4)) -> 779210,
    List((67L, 0), (7L, 1), (59L, 3), (61L, 4)) -> 1261476
  )
}
