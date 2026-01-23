package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day23 extends Day:

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
  case class Toggle(offset: Symbol)                         extends Instruction
  // extra instructions for part 2!
  case class Add(source: Symbol, destination: Symbol) extends Instruction
  case class Mul(source: Symbol, destination: Symbol) extends Instruction

  object Instruction:
    def apply(s: String): Instruction = s match
      case s"cpy $src $dst" => Copy(Symbol(src), Symbol(dst))
      case s"inc $reg"      => Inc(Symbol(reg))
      case s"dec $reg"      => Dec(Symbol(reg))
      case s"jnz $reg $off" => JumpNotZero(Symbol(reg), Symbol(off))
      case s"tgl $off"      => Toggle(Symbol(off))
      // extra
      case s"add $src $dst" => Add(Symbol(src), Symbol(dst))
      case s"mul $src $dst" => Mul(Symbol(src), Symbol(dst))

  def toggle(ins: Instruction): Instruction = ins match
    case Inc(x)            => Dec(x)
    case Dec(x)            => Inc(x)
    case Toggle(x)         => Inc(x)
    case JumpNotZero(x, y) => Copy(x, y)
    case Copy(x, y)        => JumpNotZero(x, y)
    // extra
    case Add(_, _) => throw new Exception("not toggleable")
    case Mul(_, _) => throw new Exception("not toggleable")

  lazy val input: IndexedSeq[Instruction] = loadInput().view.map(Instruction.apply).toIndexedSeq

  case class Machine(curr: Int, reg: IndexedSeq[Int], prg: IndexedSeq[Instruction]):
    def valueOf(symbol: Symbol): Int = symbol match
      case Val(x) => x
      case Reg(x) => reg(x)

    def step(): Option[Machine] =
      lazy val skip = this.copy(curr + 1)
      if curr < 0 || curr >= prg.size then None
      else
        prg(curr) match
          case Copy(src, Reg(dst))                      => Some(this.copy(curr + 1, reg.updated(dst, valueOf(src))))
          case Copy(_, _)                               => Some(skip)
          case Inc(Reg(dst))                            => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) + 1)))
          case Inc(_)                                   => Some(skip)
          case Dec(Reg(dst))                            => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) - 1)))
          case Dec(_)                                   => Some(skip)
          case JumpNotZero(src, j) if valueOf(src) != 0 => Some(this.copy(curr + valueOf(j)))
          case JumpNotZero(_, _)                        => Some(skip)
          case Toggle(src)                              =>
            // println(s"toggled ${valueOf(src)}")
            val i = curr + valueOf(src)
            if i < 0 || i >= prg.size then Some(skip)
            else Some(this.copy(curr + 1, reg, prg.updated(i, toggle(prg(i)))))
          // extra
          case Add(src, Reg(dst)) => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) + valueOf(src))))
          case Add(_, _)          => Some(skip)
          case Mul(src, Reg(dst)) => Some(this.copy(curr + 1, reg.updated(dst, reg(dst) * valueOf(src))))
          case Mul(_, _)          => Some(skip)

    def run(): Iterator[Machine] =
      Iterator.unfold(this):
        _.step().map(x => (x, x))

  lazy val part1 =
    Machine(0, IndexedSeq(7, 0, 0, 0), input)
      .run()
      .lastOption
      .get
      .reg(0)

  lazy val part2 =
    // Running the program as given only takes about a minute, but we can do better.
    // We're clued in that the program contains a loop which accomplishes
    // multiplication. We can inspect the program to figure this out and
    // replace those bits with new, more powerful instructions.
    // As it turns out, supporting add and multiply makes it plenty speedy.
    // I suspect you could break this down even further, but we're going for
    // good enough, not perfect.

    // Some by-hand analysis, starting with a=12...

    // cpy a b        12,12,0,0
    // dec b          12,11,0,0

    // cpy a d        12,11,0,12                                      }
    // cpy 0 a        0,11,0,12                                       }
    // cpy b c        0,11,11,12                                      }
    // inc a          1,11,11,12                }                     }
    // dec c          1,11,10,12                }- equiv: add c to a  }
    // jnz c -2       if c not zero, go back 2  }         set c to 0  }- equiv:
    // dec d          11,11,0,11                                      }  set a to the value a * b
    // jnz d -5       if d not zero, go back 5                        }  and set c and d to 0

    // dec b          132,10,0,0
    // cpy b c        132,10,10,0
    // cpy c d        132,10,10,10

    // dec d          132,10,10,9               }
    // inc c          132,10,11,9               }- equiv: add d to c
    // jnz d -2       if d not zero, go back 2  }         set d to 0

    // tgl c
    // cpy -16 c      this line with the next (until toggled...)
    // jnz 1 c        jumps back to index 2; will need to adjust this offset

    // cpy 78 c       the rest of this will have every other line toggled
    // jnz 70 d       by the time we break out of the previous loop
    // inc a          (some println debugging showed this pattern)
    // inc d          thus this just adds 78 * 80 to register a
    // jnz d -2
    // inc c
    // jnz c -5

    val prg2 = IndexedSeq(
      "cpy a b",
      "dec b",
      "mul b a", // first replacement (safe to skip setting 0s)
      "dec b",
      "cpy b c",
      "cpy c d",
      "add d c", // second replacement (safe to skip setting 0s)
      "tgl c",
      "cpy -7 c", // corrected jump offset
      "jnz 1 c",
      "add 5460 a", // third replacement
    ).map(Instruction.apply)

    Machine(0, IndexedSeq(12, 0, 0, 0), prg2)
      .run()
      .lastOption
      .get
      .reg(0)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
