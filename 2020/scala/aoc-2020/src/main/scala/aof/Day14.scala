package aof

import scala.annotation.tailrec

object Day14 extends Day with App {

  type Memory = Map[Long, Long]

  val day: String = "day14.txt"

  val program: List[CodeSegment] = loadProgram

  val memory = Map.empty[Int, Long]

  def loadProgram: List[CodeSegment] = {

    def go(mask: BitMask, xs: List[String], instr: List[WriteInstruction], acc: List[CodeSegment]): List[CodeSegment] = xs match {
      case h :: t if h.startsWith("mask") => go(BitMask.from(h), t, List.empty, CodeSegment(mask, instr.reverse) :: acc)
      case h :: t                         => go(mask, t, WriteInstruction.from(h) :: instr, acc)
      case Nil if instr.isEmpty           => acc
      case Nil                            => CodeSegment(mask, instr.reverse) :: acc
    }

    go(BitMask.from(lines.head), lines.tail, List.empty, List.empty).reverse

  }

  @tailrec
  def run(memory: Memory, program: List[CodeSegment])(f: (Memory, BitMask, WriteInstruction) => Memory): Memory = program match {
    case h :: t =>
      val newMem = h.ins.foldLeft(memory) { (mem, instr) =>
        f(mem, h.mask, instr)
      }
      run(newMem, t)(f)
    case Nil => memory
  }

  def decoderFun(mem: Memory, mask: BitMask, instr: WriteInstruction): Memory = {
    val base = instr.address | mask.init
    val addresses: List[Long] = mask.floatingMasks.map { bits =>
      val (zeros, ones) = maskFrom(bits)
      (base & zeros) | ones
    }
    addresses.foldLeft(mem)((m, address) => m.updated(address, instr.value))
  }

  def solutionPartA: String = {
    val memory = run(Map.empty, program) { (mem, mask, instr) =>
      mem.updated(instr.address, mask.init | (instr.value & mask.mask))
    }
    memory.values.sum.toString
  }

  def solutionPartB: String = run(Map.empty, program)(decoderFun).values.sum.toString

  case class CodeSegment(mask: BitMask, ins: List[WriteInstruction])

  case class BitMask(underlying: String) {

    lazy val (mask, init): (Long, Long) = {
      import java.lang
      val init = lang.Long.parseUnsignedLong(underlying.replace('X', '0'), 2)
      val mask = lang.Long.parseUnsignedLong(underlying.replace('1', '0').replace('X', '1'), 2)
      (mask, init)
    }

    def floatingMasks: List[List[(Int, Boolean)]] =
      for {
        ones <- subsetsOf(floating).toList
      } yield floating.map(pos => pos -> ones.contains(pos))

    def subsetsOf(xs: List[Int]): Set[Set[Int]] = xs match {
      case h :: Nil => Set(Set(h), Set.empty)
      case h :: t   => subsetsOf(t).flatMap(s => Set(s, s + h))
      case Nil      => Set.empty
    }

    val floating: List[Int] = underlying.toList.reverse.zipWithIndex.filter(_._1 == 'X').map(_._2)
  }

  // zeros -> ones
  def maskFrom(xs: List[(Int, Boolean)]): (Long, Long) = {
    val ones = xs.foldLeft(0L) {
      case (acc, (pos, b)) if b => acc | (1L << pos)
      case (acc, _)             => acc
    }
    val zeros = xs.foldLeft(~0L) {
      case (acc, (pos, _)) => acc ^ (1L << pos)
    }
    val mask36 = 0xfffffffffL
    (zeros & mask36, ones & mask36)
  }

  object BitMask {
    def from(s: String): BitMask = BitMask(s.stripPrefix("mask = "))
  }

  case class WriteInstruction(address: Long, value: Long)

  object WriteInstruction {
    def from(s: String): WriteInstruction = {
      val arr = s.split('=')
      val value = arr(1).trim.toLong
      val memory = arr(0).trim.stripPrefix("mem[").stripSuffix("]").toLong
      WriteInstruction(memory, value)
    }
  }

  run()
}
