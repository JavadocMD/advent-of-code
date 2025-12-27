package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day07 extends Day:

  sealed trait Gate:
    val out: String
    def byOut = out -> this

  case class And(in1: String, in2: String, out: String)     extends Gate
  case class Or(in1: String, in2: String, out: String)      extends Gate
  case class LShift(in: String, shift: String, out: String) extends Gate
  case class RShift(in: String, shift: String, out: String) extends Gate
  case class Not(in: String, out: String)                   extends Gate
  case class Assign(in: String, out: String)                extends Gate

  type Input = List[Gate]

  def parse(xs: List[String]): Input = xs.map:
    case s"$in1 AND $in2 -> $out"     => And(in1, in2, out)
    case s"$in1 OR $in2 -> $out"      => Or(in1, in2, out)
    case s"$in LSHIFT $shift -> $out" => LShift(in, shift, out)
    case s"$in RSHIFT $shift -> $out" => RShift(in, shift, out)
    case s"NOT $in -> $out"           => Not(in, out)
    case s"$in -> $out"               => Assign(in, out)

  lazy val input = parse(loadInput().toList)

  def evaluate(out: String, gates: List[Gate]): Int =
    import scala.collection.{mutable as m}
    val gatesByOutput = gates.map(_.byOut).toMap
    val memo          = m.Map.empty[String, Int]

    def eval(out: String): Int =
      if memo.contains(out) then memo(out)
      else
        def eval2(nameOrValue: String): Int = nameOrValue.toIntOption.getOrElse(eval(nameOrValue))
        val rawResult = gatesByOutput(out) match
          case And(in1, in2, _)     => eval2(in1) & eval2(in2)
          case Or(in1, in2, _)      => eval2(in1) | eval2(in2)
          case LShift(in, shift, _) => eval2(in) << shift.toInt
          case RShift(in, shift, _) => eval2(in) >> shift.toInt
          case Not(in, _)           => ~eval2(in)
          case Assign(in, _)        => eval2(in)
        val result = rawResult & 0xffff // constrain to 16-bit values
        memo(out) = result
        result

    eval(out)

  lazy val part1 = evaluate("a", input)

  lazy val part2 =
    val input2 = input.updated(
      index = input.indexWhere(_.out == "b"),
      elem = Assign(part1.toString, "b"),
    )
    evaluate("a", input2)

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
