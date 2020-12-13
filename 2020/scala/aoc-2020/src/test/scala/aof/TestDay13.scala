package aof

import aof.Day13.Id
import aof.TestDay13.Examples
import org.scalatest.funsuite.AnyFunSuite

class TestDay13 extends AnyFunSuite {

  test("test timestampOf") {
//    testAlg(Day13.timestampOfSlow)
    testAlg(Day13.timestampOfFast)
  }

  def testAlg(f: (List[(Id, Int)], Long, Option[Id]) => Long): Unit =
    Examples.foreach {
      case (xs, (start, ts)) =>
        println(s"$xs -> $ts")
        assert(f(xs, start, None: Option[Id]) === ts)
    }

}

object TestDay13 {
  val Examples = List(
    List((7L, 0), (13L, 1), (59L, 4), (31L, 6), (19L, 7)) -> (0L, 1068781),
    List((17L, 0), (13L, 2), (19L, 3)) -> (0L, 3417),
    List((1789L, 0), (37L, 1), (47L, 2), (1889L, 3)) -> (0L, 1202161486),
    List((67L, 0), (7L, 1), (59L, 2), (61L, 3)) -> (0L, 754018),
    List((67L, 0), (7L, 2), (59L, 3), (61L, 4)) -> (0L, 779210),
    List((17L,0), (41L,7), (643L,17), (23L,25), (13L,30), (29L,46), (433L,48), (37L,54), (19L,67)) -> (760071380521445L, 760171380521445L),
    List((67L, 0), (7L, 1), (59L, 3), (61L, 4)) -> (0L, 1261476)
  )
}
