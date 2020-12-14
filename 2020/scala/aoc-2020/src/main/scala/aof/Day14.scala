package aof

import scala.annotation.tailrec

object Day14 extends Day with App {

  type Memory = Map[Int, Long]

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
//        println(s"value: ${mem.get(instr.address)}")
//        println(s"mask: ${h.mask} (${h.mask.init}\t${h.mask.mask})")
        f(mem, h.mask, instr)
      }
      run(newMem, t)(f)
    case Nil => memory
  }

  def solutionPartA: String = {
    val memory = run(Map.empty, program) { (mem, mask, instr) =>
      mem.updated(instr.address, mask.init | (instr.value & mask.mask))
    }
    memory.values.sum.toString
  }

  def solutionPartB: String = ""

  case class CodeSegment(mask: BitMask, ins: List[WriteInstruction])
  case class BitMask(underlying: String) {

    lazy val (mask, init): (Long, Long) = {
      import java.lang
      val init = lang.Long.parseUnsignedLong(underlying.replace('X', '0'), 2)
//      println("init: "+underlying.replace('X', '0'))
//      println("init: "+init)
      val mask = lang.Long.parseUnsignedLong(underlying.replace('1', '0').replace('X', '1'), 2)
//      println("mask: "+underlying.replace('1', '0').replace('X', '1'))
//      println("mask: "+mask)
      (mask, init)
    }
  }

  object BitMask {
    def from(s: String): BitMask = BitMask(s.stripPrefix("mask = "))
  }

  case class WriteInstruction(address: Int, value: Long)

  object WriteInstruction {
    def from(s: String): WriteInstruction = {
      val arr = s.split('=')
      val value = arr(1).trim.toLong
      val memory = arr(0).trim.stripPrefix("mem[").stripSuffix("]").toInt
      WriteInstruction(memory, value)
    }
  }

  run()
}
