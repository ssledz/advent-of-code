package aof

import aof.IntComputer.{ReadInputOpcode, opcode}

import scala.annotation.tailrec

object Day5 extends Day {

  val day: String = "05"

  def memory: Array[Int] = lines.head.split(',').map(_.toInt)

  def solutionPartA: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(1))
    c.out.toString
  }

  def solutionPartB: String = {
    val c = new IntComputer(memory)
    c.runInterpreter(List(5))
    c.out.toString
  }

}

class IntComputer(m: Array[Int], output: Int = 0, pc: Int = 0, val halt: Boolean = false, debug: Boolean = false, trace: Boolean = false) {

  def tr(s: String): Unit = if (trace) println("tr: " + s)

  def dbg(s: String): Unit = if (debug) println("dbg: " + s)

  def out: Int = m(output)

  def waitingInput: Boolean = currentOpcode(pc) match {
    case (_, ReadInputOpcode) => true
    case _ => false
  }

  def runInterpreter(input: List[Int]): IntComputer = run(pc, input) match {
    case (pc, halted) => new IntComputer(m, output, pc, halted, debug, trace)
  }

  private def currentOpcode(pc: Int): (Option[(Int, Int, Int)], Int) = opcode(m(pc))

  @tailrec
  private def run(pc: Int, input: List[Int]): (Int, Boolean) = {

    tr(s"pc=$pc, memory=${m.toList}")

    def readValue(mode: Int, address: Int): Int =
      if (mode == 0) m(m(address)) else m(address)

    def arithmeticOp(mode: (Int, Int, Int), f: (Int, Int) => Int): Unit = mode match {
      case (_, b, c) =>
        val x = readValue(c, pc + 1)
        val y = readValue(b, pc + 2)
        val dest = m(pc + 3)
        m(dest) = f(x, y)
        tr(s"op: m($dest) = $x ? $y = ${f(x, y)}")
    }

    def readInput: Unit = m(m(pc + 1)) = input.head

    def writeOutput(mode: (Int, Int, Int)): Unit = mode match {
      case (_, _, 0) => m(output) = m(m(pc + 1))
      case (_, _, 1) => m(output) = m(pc + 1)
    }

    def jumpIf(mode: (Int, Int, Int), f: (Int, Int) => Boolean): Int = mode match {
      case (_, b, c) =>
        if (f(readValue(c, pc + 1), 0)) readValue(b, pc + 2) else pc + 3
    }

    def jumpIfTrue(mode: (Int, Int, Int)): Int = jumpIf(mode, _ != _)

    def jumpIfFalse(mode: (Int, Int, Int)): Int = jumpIf(mode, _ == _)

    def condition(mode: (Int, Int, Int), f: (Int, Int) => Boolean): Unit = mode match {
      case (_, b, c) =>
        val first = readValue(c, pc + 1)
        val second = readValue(b, pc + 2)
        m(m(pc + 3)) = if (f(first, second)) 1 else 0
    }

    def lessThan(mode: (Int, Int, Int)): Unit = condition(mode, _ < _)

    def equals(mode: (Int, Int, Int)): Unit = condition(mode, _ == _)

    val opc = currentOpcode(pc)

    tr(s"opcode: $opc")

    opc match {
      case (Some(mode), 1) =>
        arithmeticOp(mode, _ + _)
        run(pc + 4, input)
      case (Some(mode), 2) =>
        arithmeticOp(mode, _ * _)
        run(pc + 4, input)
      case (_, ReadInputOpcode) =>
        if (!input.isEmpty) {
          readInput
          run(pc + 2, input.tail)
        } else {
          (pc, false)
        }
      case (Some(mode), 4) =>
        dbg(s"previous test result (pc=$pc): " + output)
        writeOutput(mode)
        run(pc + 2, input)
      case (Some(mode), 5) => run(jumpIfTrue(mode), input)
      case (Some(mode), 6) => run(jumpIfFalse(mode), input)
      case (Some(mode), 7) =>
        lessThan(mode)
        run(pc + 4, input)
      case (Some(mode), 8) =>
        equals(mode)
        run(pc + 4, input)
      case (_, 99) => (pc, true)
      case oc => throw new IllegalStateException(s"Illegal op code=$oc, pc=$pc")
    }
  }

}

object IntComputer {

  val ReadInputOpcode = 3

  private def toInt(c: Char): Int = c - '0'

  private def toInt(a: Char, b: Char): Int = s"$a$b".toInt

  def opcode(n: Int): (Option[(Int, Int, Int)], Int) = {
    if (n == 3 || n == 99) {
      (None, n)
    } else {
      n.toString.toList match {
        case a :: b :: c :: d :: e :: Nil => (Some((toInt(a), toInt(b), toInt(c))), toInt(d, e))
        case b :: c :: d :: e :: Nil => (Some(0, toInt(b), toInt(c)), toInt(d, e))
        case c :: d :: e :: Nil => (Some(0, 0, toInt(c)), toInt(d, e))
        case d :: e :: Nil => (Some(0, 0, 0), toInt(d, e))
        case e :: Nil => (Some(0, 0, 0), toInt(e))
      }
    }
  }


}

object Day5App extends App {

  println("SolutionPartA: " + Day5.solutionPartA)
  println("SolutionPartB: " + Day5.solutionPartB)

}
