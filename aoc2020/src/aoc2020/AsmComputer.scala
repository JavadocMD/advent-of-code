package aoc2020

class AsmComputer(val program: IndexedSeq[AsmComputer.Op], pointer0: Int = 0, acc0: Int = 0):
  import AsmComputer._
  import AsmComputer.Op._

  private[this] var _pointer = pointer0
  private[this] var _acc     = acc0

  def pointer: Int = _pointer
  def acc: Int     = _acc

  def complete: Boolean = pointer >= program.size

  def step(): Unit = program(_pointer) match
    case Nop(_) => _pointer += 1
    case Acc(v) => _pointer += 1; _acc += v
    case Jmp(v) => _pointer += v

object AsmComputer:
  import fastparse._
  import fastparse.NoWhitespace._
  import fastparse.Parsed.{Success, Failure}

  sealed trait Op
  object Op:
    case class Acc(value: Int) extends Op
    case class Jmp(value: Int) extends Op
    case class Nop(value: Int) extends Op

  type Program = IndexedSeq[Op]

  def num[$: P] = P((CharIn("+\\-") ~ CharIn("0-9").rep).!).map(_.toInt)
  def acc[$: P] = P(Start ~ "acc " ~ num ~ End).map(Op.Acc.apply)
  def jmp[$: P] = P(Start ~ "jmp " ~ num ~ End).map(Op.Jmp.apply)
  def nop[$: P] = P(Start ~ "nop " ~ num ~ End).map(Op.Nop.apply)
  def op[$: P]  = P[Op](acc | jmp | nop)

  def parseProgram(input: IndexedSeq[String]): IndexedSeq[Op] =
    input.map { s =>
      parse(s, op(_)) match
        case Success(x, _) => x
        case _             => throw new Exception(s"Unable to parse $s")
    }
