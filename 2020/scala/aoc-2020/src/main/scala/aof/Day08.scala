package aof

import scala.annotation.tailrec

object Day08 extends Day with App {

  val day: String = "day08.txt"

  val ins = lines.map(Instruction.from)

  @tailrec
  def sim(c: Computer, visited: Set[Int], state: CState): CState = {
    val newState = c.run(state)
    if (newState.term || visited.contains(newState.pc))
      newState
    else sim(c, visited + newState.pc, newState)
  }

  def solutionPartA: String = sim(Computer(ins.toArray), Set.empty, CState(0, 0, term = false)).acc.toString

  def solutionPartB: String = {
    val toReplace = ins.zipWithIndex
      .filter(in => in._1.opcode == "nop" || in._1.opcode == "jmp")
      .filterNot(in => in._1.opcode == "nop" && in._1.arg == 0)
      .map(_._2)
    def swap(arr: Array[Instruction], i: Int): Array[Instruction] = {
      val opcode = arr(i).opcode
      arr(i) = arr(i).copy(opcode = if (opcode == "nop") "jmp" else "nop")
      arr
    }
    val maybeRep = toReplace.find { rep =>
      val p = swap(ins.toArray, rep)
      val state = sim(Computer(p), Set.empty, CState(0, 0, term = false))
      state.term
    }
    maybeRep.map(rep => sim(Computer(swap(ins.toArray, rep)), Set.empty, CState(0, 0, term = false)).acc).toString
  }

  run()

  case class Computer(program: Array[Instruction]) {
    def term(pc: Int): Boolean = pc >= program.length
    def run(s: CState): CState = {
      val Instruction(opcode, arg) = program(s.pc)
      val CState(pc, acc, _) = s
      opcode match {
        case "nop" => CState(pc + 1, acc, term(pc + 1))
        case "acc" => CState(pc + 1, acc + arg, term(pc + 1))
        case "jmp" => CState(pc + arg, acc, term(pc + arg))
      }
    }
  }

  case class Instruction(opcode: String, arg: Int)
  case class CState(pc: Int, acc: Int, term: Boolean)

  object Instruction {
    private val r = """([^ ]+) ([+-])(\d+)""".r
    def from(s: String): Instruction = s match {
      case r(opcode, s, arg) => Instruction(opcode, arg.toInt * (if (s == "-") -1 else 1))
    }
  }

}
