package aof

import aof.IntComputer.opcode

object Day5 extends Day {

  val day: String = "05"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def solutionPartA: String = {
    val c = new IntComputer(memory, 1)
    c.runInterpreter
    c.out.toString
  }

  def solutionPartB: String = {
    ""
  }

}

class IntComputer(m: Array[Int], input: Int, output: Int = 0, debug: Boolean = false, trace: Boolean = false) {

  def tr(s: String): Unit = if (trace) println("tr: " + s)

  def dbg(s: String): Unit = if (debug) println("dbg: " + s)

  def out: Int = m(output)

  def runInterpreter: Array[Int] = run(0)

  private def run(pc: Int): Array[Int] = {

    tr(s"pc=$pc, memory=${m.toList}")

    def readValue(mode: Int, address: Int): Int =
      if (mode == 0) m(m(address)) else m(address)

    def arthmeticOp(mode: (Int, Int, Int), f: (Int, Int) => Int): Unit = mode match {
      case (_, b, c) =>
        val x = readValue(c, pc + 1)
        val y = readValue(b, pc + 2)
        val dest = m(pc + 3)
        m(dest) = f(x, y)
        tr(s"op: m($dest) = $x ? $y = ${f(x, y)}")
    }

    val opc = opcode(m(pc))

    tr(s"opcode: $opc")

    opc match {
      case (Some(mode), 1) => arthmeticOp(mode, _ + _)
        run(pc + 4)
      case (Some(mode), 2) => arthmeticOp(mode, _ * _)
        run(pc + 4)
      case (mode, 3) =>
        m(m(pc + 1)) = input
        run(pc + 2)
      case (mode, 4) =>
        dbg(s"previous test result (pc=$pc): " + output)
        mode match {
          case Some((_, _, 0)) => m(output) = m(m(pc + 1))
          case Some((_, _, 1)) => m(output) = m(pc + 1)
        }
        run(pc + 2)
      case (_, 99) => m
      case oc => throw new IllegalStateException(s"Illegal op code=$oc, pc=$pc")
    }
  }

}

object IntComputer {

  private def toInt(c: Char): Int = c - '0'

  def opcode(n: Int) = {
    if (n == 3 || n == 99) {
      (None, n)
    } else {
      n.toString.toList match {
        case a :: b :: c :: d :: e :: Nil => (Some((toInt(a), toInt(b), toInt(c))), ("" + d + e).toInt)
        case b :: c :: d :: e :: Nil => (Some(0, toInt(b), toInt(c)), ("" + d + e).toInt)
        case c :: d :: e :: Nil => (Some(0, 0, toInt(c)), ("" + d + e).toInt)
        case '1' :: Nil => (Some(0, 0, 0), 1)
        case '2' :: Nil => (Some(0, 0, 0), 2)
        case '4' :: Nil => (Some(0, 0, 0), 4)
      }
    }
  }


}

object Day5App extends App {

  println("SolutionPartA: " + Day5.solutionPartA)
  //  println("SolutionPartB: " + Day5.solutionPartB)

  //    val memory = Array(1002, 4, 3, 4, 33)
  //  val memory = Array(1101, 100, -1, 4, 0)
  //  val memory = Array(3, 0, 4, 0, 99)
  //  val c = new IntComputer(memory, 11)
  //  c.runInterpreter
  //  println(memory.toList)
}
