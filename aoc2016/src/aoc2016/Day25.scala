package aoc2016

import scala.annotation.tailrec
import aoc.Day
import scala.math.abs

object Day25 extends Day:

  sealed trait Symbol
  case class Val(value: Int) extends Symbol
  case class Reg(index: Int) extends Symbol

  object Symbol:
    def apply(s: String): Symbol = s.toIntOption match
      case Some(x) => Val(x)
      case None    => Reg(s.head - 'a')

  sealed trait Instruction
  case class Copy(source: Symbol, destination: Symbol)      extends Instruction
  case class Inc(register: Symbol)                          extends Instruction
  case class Dec(register: Symbol)                          extends Instruction
  case class JumpNotZero(predicate: Symbol, offset: Symbol) extends Instruction
  case class Out(symbol: Symbol)                            extends Instruction

  object Instruction:
    def apply(s: String): Instruction = s match
      case s"cpy $src $dst" => Copy(Symbol(src), Symbol(dst))
      case s"inc $reg"      => Inc(Symbol(reg))
      case s"dec $reg"      => Dec(Symbol(reg))
      case s"jnz $reg $off" => JumpNotZero(Symbol(reg), Symbol(off))
      case s"out $sym"      => Out(Symbol(sym))

  case class Machine(curr: Int, reg: IndexedSeq[Int], out: Option[Int], prg: IndexedSeq[Instruction]):
    def valueOf(symbol: Symbol): Int = symbol match
      case Val(x) => x
      case Reg(x) => reg(x)

    def step(): Option[Machine] =
      lazy val skip = this.copy(curr + 1, reg, None)
      if curr < 0 || curr >= prg.size then None
      else
        prg(curr) match
          case Copy(src, Reg(dst)) => Some(this.copy(curr + 1, reg.updated(dst, valueOf(src)), None))
          case Copy(_, _)          => Some(skip)
          case Inc(Reg(dst))       => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) + 1), None))
          case Inc(_)              => Some(skip)
          case Dec(Reg(dst))       => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) - 1), None))
          case Dec(_)              => Some(skip)
          case JumpNotZero(src, j) if valueOf(src) != 0 => Some(this.copy(curr + valueOf(j), reg, None))
          case JumpNotZero(_, _)                        => Some(skip)
          case Out(sym)                                 => Some(this.copy(curr + 1, reg, Some(valueOf(sym))))

  lazy val input: IndexedSeq[Instruction] = loadInput().view.map(Instruction.apply).toIndexedSeq

  def initial(a: Int) = Machine(0, IndexedSeq(a, 0, 0, 0), None, input)

  @tailrec
  def recurse(machine: Machine, expect: Int, iterations: Int = 1_000_000): Boolean =
    if iterations == 0 then true
    else
      machine.step() match
        // program terminates, not a clock!
        case None => false
        // no output, continue
        case Some(next) if next.out == None => recurse(next, expect, iterations - 1)
        // output doesn't match expectation, not a clock!
        case Some(next) if next.out.get == expect => false
        // output matches expectations, continue
        case Some(next) => recurse(next, abs(expect - 1), iterations - 1)

  lazy val part1 =
    // With experimentation, it turns out the sequence starts with 1, not 0.
    val trials = Iterator.from(0).map(i => recurse(initial(i), expect = 1))
    trials.indexWhere(_ == true)

  lazy val part2 = "MERRY CHRISTMAS"

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
