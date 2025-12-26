package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._

object Day23 extends Day:

  sealed trait Instr
  case class Hlf(reg: Int)              extends Instr
  case class Tpl(reg: Int)              extends Instr
  case class Inc(reg: Int)              extends Instr
  case class Jmp(offset: Int)           extends Instr
  case class Jie(reg: Int, offset: Int) extends Instr
  case class Jio(reg: Int, offset: Int) extends Instr

  type Input = IndexedSeq[Instr]

  extension (s: String) def toReg: Int = s.head - 'a'

  def parse(xs: List[String]): Input = xs.view
    .map:
      case s"hlf $reg"       => Hlf(reg.toReg)
      case s"tpl $reg"       => Tpl(reg.toReg)
      case s"inc $reg"       => Inc(reg.toReg)
      case s"jmp $off"       => Jmp(off.toInt)
      case s"jie $reg, $off" => Jie(reg.toReg, off.toInt)
      case s"jio $reg, $off" => Jio(reg.toReg, off.toInt)
    .toIndexedSeq

  case class Machine(curr: Int, reg: IndexedSeq[Long]):
    def step(instructions: IndexedSeq[Instr]): Option[Machine] =
      if curr < 0 || curr >= instructions.size then None
      else
        instructions(curr) match
          case Hlf(r) => Some(this.copy(curr + 1, reg.updated(r, reg(r) / 2)))
          case Tpl(r) => Some(this.copy(curr + 1, reg.updated(r, reg(r) * 3)))
          case Inc(r) => Some(this.copy(curr + 1, reg.updated(r, reg(r) + 1)))
          case Jmp(j) => Some(this.copy(curr + j))
          case Jie(r, j) if reg(r) % 2 == 0 => Some(this.copy(curr + j))
          case Jie(r, _)                => Some(this.copy(curr + 1))
          case Jio(r, j) if reg(r) == 1 => Some(this.copy(curr + j))
          case Jio(r, _)                => Some(this.copy(curr + 1))

    def run(instructions: IndexedSeq[Instr]): Iterator[Machine] =
      Iterator.unfold(this)(_.step(input).map(x => (x, x)))

  lazy val input = parse(loadInput().toList)

  lazy val part1 =
    Machine(0, Vector(0, 0))
      .run(input)
      .lastOption
      .map(_.reg("b".toReg))
      .get

  lazy val part2 =
    Machine(0, Vector(1, 0))
      .run(input)
      .lastOption
      .map(_.reg("b".toReg))
      .get

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
