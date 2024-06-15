package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day21 extends Day:
  enum Operator:
    case Add, Sub, Mul, Div

    def eval(lhs: Long, rhs: Long): Long = this match
      case Add => lhs + rhs
      case Sub => lhs - rhs
      case Mul => lhs * rhs
      case Div => lhs / rhs

    def solveLeft(result: Long, operand: Long): Long = this match
      case Add => result - operand
      case Sub => result + operand
      case Mul => result / operand
      case Div => result * operand

    def solveRight(result: Long, operand: Long): Long = this match
      case Add => result - operand
      case Sub => operand - result
      case Mul => result / operand
      case Div => operand / result

  object Operator:
    def unapply(s: String): Option[Operator] = s match
      case "+" => Some(Add)
      case "-" => Some(Sub)
      case "*" => Some(Mul)
      case "/" => Some(Div)
      case _   => None
  import Operator._

  enum Monkey:
    case Opn(op: Operator, lhs: Monkey, rhs: Monkey)
    case Num(value: Long)
    case Hmn // human, for part 2

    def eval: Long = this match
      case Num(value)        => value
      case Opn(op, lhs, rhs) => op.eval(lhs.eval, rhs.eval)
      case Hmn               => throw new Exception("unhandled case: Human")

    def involvesHuman: Boolean = this match
      case Hmn              => true
      case Num(_)           => false
      case Opn(_, lhs, rhs) => lhs.involvesHuman || rhs.involvesHuman

    def solve(result: Long): Long = this match
      case Hmn                         => result
      case Opn(op, Num(operand), term) => term.solve(op.solveRight(result, operand))
      case Opn(op, term, Num(operand)) => term.solve(op.solveLeft(result, operand))
      case _                           => throw new Exception(s"unhandled case $this")
  import Monkey._

  type Input = Map[String, String] // a map of monkey IDs to their operation description (still a String)

  def parse(xs: Iterator[String]): Input = xs.map { case s"$id: $rest" => (id -> rest) }.toMap

  def interpretMonkeys(ids: Input, withHuman: Boolean): Monkey =
    def recurse(id: String): Monkey = (id, ids(id)) match
      case ("humn", _) if withHuman => Hmn
      case (_, s"${LongP(value)}")  => Num(value)
      case (_, s"$left ${Operator(op)} $right") =>
        val lhs = recurse(left)
        val rhs = recurse(right)
        (lhs, rhs) match
          case (Num(x), Num(y)) => Num(op.eval(x, y))
          case (lhs, rhs)       => Opn(op, lhs, rhs)
    recurse("root")

  def part1(input: Input) = interpretMonkeys(input, withHuman = false).eval

  def part2(input: Input) =
    val Opn(_, left, right) = interpretMonkeys(input, withHuman = true): @unchecked
    if left.involvesHuman ^ right.involvesHuman == false then
      throw new Exception("unexpected case: only one side can involve the human")
    val (tree, value) = if left.involvesHuman then (left, right.eval) else (right, left.eval)
    tree.solve(value)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
