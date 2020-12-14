package aof

import scala.annotation.tailrec

object Day14 extends Day with App {

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
  def run(memory: Map[Int, Long], program: List[CodeSegment]): Map[Int, Long] = program match {
    case h :: t =>
      val newMem = h.ins.foldLeft(memory) { (mem, instr) =>
        println(s"value: ${mem.get(instr.address)}")
        println(s"mask: ${h.mask} (${h.mask.init}\t${h.mask.mask})")

        val x= mem.updated(instr.address, h.mask.init | (instr.value & h.mask.mask))

//        val x= mem.updatedWith(instr.address) {
//          case Some(_) => Some(h.mask.init | (instr.value & h.mask.mask))
//          case None => Some(h.mask.init)
//        }
        println(s"result: ${x(instr.address)}")
        x
      }
      run(newMem, t)
    case Nil => memory
  }

  def solutionPartA: String = {
    println(program)
//    println(program.head.mask.underlying)
//    println(program.head.mask.mask)
//    println(program.head.mask.init)

    val mem = run(Map.empty, program)
    println(mem)

    mem.values.sum.toString
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
