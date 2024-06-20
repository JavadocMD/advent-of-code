package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day08 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  // Run the computer until termination or until an infinite loop is detected.
  // Return true if program terminates.
  def run(computer: AsmComputer): Boolean =
    var seen = Set.empty[Int] // which instructions have we executed?
    var curr = computer.pointer
    while (!computer.complete && !seen.contains(curr)) {
      seen += curr
      computer.step()
      curr = computer.pointer
    }
    computer.complete

  def part1(program: AsmComputer.Program): Int =
    val computer = new AsmComputer(program)
    run(computer)
    computer.acc

  def part2(program: AsmComputer.Program): Int =
    import AsmComputer.Op._

    case class Patch(val op: AsmComputer.Op, val i: Int)
    def patched(p: Patch) = program.updated(p.i, p.op)

    // Find every instruction that we could effectively flip (ignore `jmp +0`).
    // As tuples with the alternate instruction and its index.
    val alternatives = program.zipWithIndex.flatMap[Patch]({
      case (Nop(x), i) if x > 0 => Some(Patch(Jmp(x), i))
      case (Jmp(x), i)          => Some(Patch(Nop(x), i))
      case _                    => None
    })

    // Find the first alternative which results in a terminating program.
    mapFind((p: Patch) => {
      val c = new AsmComputer(patched(p))
      if (run(c)) Some(c.acc) else None
    })(alternatives).get

  final def main(args: Array[String]): Unit =
    val in      = parse(loadInput())
    val program = AsmComputer.parseProgram(in)
    solveP1(() => part1(program))
    solveP2(() => part2(program))
