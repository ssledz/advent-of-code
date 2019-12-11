package aof

import aof.IntComputer._

import scala.annotation.tailrec

case class IntComputer(m: Array[Long], pc: Int = 0, rb: Int = 0, output: List[Long] = List.empty, halt: Boolean = false, debug: Boolean = false, trace: Boolean = false) {
  self =>

  def tr(s: String): Unit = if (trace) println("tr: " + s)

  def dbg(s: String): Unit = if (debug) println("dbg: " + s)

  def out: (Option[Long], IntComputer) = output.headOption match {
    case o@Some(_) => (o, self.copy(output = output.tail))
    case None => (None, self)
  }

  def waitingInput: Boolean = currentOpcode(pc) match {
    case (_, ReadInputOpcode) => true
    case _ => false
  }

  def runInterpreter(input: List[Int]): IntComputer = run(pc, rb, input, output) match {
    case (pc, rb, output, halted) => new IntComputer(m, pc, rb, output, halted, debug, trace)
  }

  private def currentOpcode(pc: Int): (Option[(Int, Int, Int)], Int) = opcode(m(pc).toInt)

  @tailrec
  private def run(pc: Int, rb: Int, input: List[Int], output: List[Long]): (Int, Int, List[Long], Boolean) = {

    tr(s"pc=$pc, rb=$rb, input=$input, output=$output, memory=${m.toList}")

    def resolveAddress(mode: Int, address: Int): Int = mode match {
      case 0 => m(address).toInt
      case 1 => address
      case 2 => rb + m(address).toInt
    }

    def readValue(mode: Int, address: Int): Long = m(resolveAddress(mode, address))

    def arithmeticOp(mode: (Int, Int, Int), f: (Long, Long) => Long): Unit = mode match {
      case (a, b, c) =>
        val x = readValue(c, pc + 1)
        val y = readValue(b, pc + 2)
        val dest = resolveAddress(a, pc + 3)
        m(dest) = f(x, y)
        tr(s"arithmeticOp: m($dest) = $x ? $y = ${f(x, y)}")
    }

    def readInput(mode: Int): Unit = m(resolveAddress(mode, pc + 1)) = input.head

    def writeOutput(mode: (Int, Int, Int)): Long = mode match {
      case (_, _, c) => {
        val out = readValue(c, pc + 1)
        tr(s"writeOutput op: readValue${(c, pc + 1)} => ($out)")
        out
      }
    }

    def jumpIf(mode: (Int, Int, Int), f: (Long, Long) => Boolean): Int = mode match {
      case (_, b, c) =>
        if (f(readValue(c, pc + 1), 0)) readValue(b, pc + 2).toInt else pc + 3
    }

    def jumpIfTrue(mode: (Int, Int, Int)): Int = jumpIf(mode, _ != _)

    def jumpIfFalse(mode: (Int, Int, Int)): Int = jumpIf(mode, _ == _)

    def condition(mode: (Int, Int, Int), f: (Long, Long) => Boolean): Unit = mode match {
      case (a, b, c) =>
        val first = readValue(c, pc + 1)
        val second = readValue(b, pc + 2)
        m(resolveAddress(a, pc + 3)) = if (f(first, second)) 1 else 0
    }

    def lessThan(mode: (Int, Int, Int)): Unit = condition(mode, _ < _)

    def equals(mode: (Int, Int, Int)): Unit = condition(mode, _ == _)

    val opc = currentOpcode(pc)

    tr(s"opcode: $opc")

    opc match {
      case (Some(mode), 1) =>
        arithmeticOp(mode, _ + _)
        run(pc + 4, rb, input, output)
      case (Some(mode), 2) =>
        arithmeticOp(mode, _ * _)
        run(pc + 4, rb, input, output)
      case (Some((_, _, c)), ReadInputOpcode) =>
        if (!input.isEmpty) {
          readInput(c)
          run(pc + 2, rb, input.tail, output)
        } else {
          (pc, rb, output, false)
        }
      case (Some(mode), 4) =>
        run(pc + 2, rb, input, writeOutput(mode) :: output)
      case (Some(mode), 5) => run(jumpIfTrue(mode), rb, input, output)
      case (Some(mode), 6) => run(jumpIfFalse(mode), rb, input, output)
      case (Some(mode), 7) =>
        lessThan(mode)
        run(pc + 4, rb, input, output)
      case (Some(mode), 8) =>
        equals(mode)
        run(pc + 4, rb, input, output)
      case (Some((_, _, c)), 9) =>
        run(pc + 2, rb + readValue(c, pc + 1).toInt, input, output)
      case (_, 99) => (pc, rb, output, true)
      case oc => throw new IllegalStateException(s"Illegal op code=$oc, pc=$pc")
    }
  }

}

object IntComputer {

  val ReadInputOpcode = 3

  def extendMemory(m: Array[Long], amount: Int = 1024): Array[Long] =
    m ++ (1 to amount).map(_ => 0L).toArray

  private def toInt(c: Char): Int = c - '0'

  private def toInt(a: Char, b: Char): Int = s"$a$b".toInt

  def opcode(n: Int): (Option[(Int, Int, Int)], Int) = {
    if (n == 99) {
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