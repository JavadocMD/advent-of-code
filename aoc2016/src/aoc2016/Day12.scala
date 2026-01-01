package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day12 extends Day:

  sealed trait Instruction
  case class CopyReg(source: Int, destination: Int)   extends Instruction
  case class CopyVal(source: Int, destination: Int)   extends Instruction
  case class Inc(register: Int)                       extends Instruction
  case class Dec(register: Int)                       extends Instruction
  case class Jump(offset: Int)                        extends Instruction
  case class JumpNotZero(predicate: Int, offset: Int) extends Instruction

  extension (s: String) def toReg: Int = s.head - 'a'

  lazy val input: IndexedSeq[Instruction] =
    loadInput().view
      .map:
        case s"cpy $src $dst" if src.head.isDigit => CopyVal(src.toInt, dst.toReg)
        case s"cpy $src $dst"                     => CopyReg(src.toReg, dst.toReg)
        case s"inc $reg"                          => Inc(reg.toReg)
        case s"dec $reg"                          => Dec(reg.toReg)
        case s"jnz 1 $off"                        => Jump(off.toInt)
        case s"jnz $reg $off"                     => JumpNotZero(reg.toReg, off.toInt)
      .toIndexedSeq

  case class Machine(curr: Int, reg: IndexedSeq[Int]):
    def step(instructions: IndexedSeq[Instruction]): Option[Machine] =
      if curr < 0 || curr >= instructions.size then None
      else
        instructions(curr) match
          case CopyReg(src, dst)                => Some(this.copy(curr + 1, reg.updated(dst, reg(src))))
          case CopyVal(value, dst)              => Some(this.copy(curr + 1, reg.updated(dst, value)))
          case Inc(r)                           => Some(this.copy(curr + 1, reg.updated(r, reg(r) + 1)))
          case Dec(r)                           => Some(this.copy(curr + 1, reg.updated(r, reg(r) - 1)))
          case Jump(j)                          => Some(this.copy(curr + j))
          case JumpNotZero(r, j) if reg(r) != 0 => Some(this.copy(curr + j))
          case JumpNotZero(r, _)                => Some(this.copy(curr + 1))

    def run(instructions: IndexedSeq[Instruction]): Iterator[Machine] =
      Iterator.unfold(this):
        _.step(instructions).map(x => (x, x))

  lazy val part1 =
    Machine(0, IndexedSeq(0, 0, 0, 0))
      .run(input)
      .lastOption
      .map(_.reg("a".toReg))
      .get

  lazy val part2 =
    Machine(0, IndexedSeq(0, 0, 1, 0))
      .run(input)
      .lastOption
      .map(_.reg("a".toReg))
      .get

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
