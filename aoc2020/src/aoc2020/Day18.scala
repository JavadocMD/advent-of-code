package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day18 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs.map(_.replaceAll(" ", ""))

  sealed trait Exp:
    def eval: Long

  case class Num(n: Long) extends Exp:
    val eval = n

  case class Mul(lhs: Exp, rhs: Exp) extends Exp:
    def eval = lhs.eval * rhs.eval

  case class Add(lhs: Exp, rhs: Exp) extends Exp:
    def eval = lhs.eval + rhs.eval

  case class Par(exp: Exp) extends Exp:
    def eval = exp.eval

  case object NilExp extends Exp:
    def eval = throw new Exception("Cannot eval NilExp")

  def toNum(chr: Char): Num = Num(chr.toLong - '0')

  sealed trait Op
  object Op:
    case object Mul  extends Op
    case object Add  extends Op
    case object None extends Op

  // For part 1: simply parse the expression as a tree going left-to-right,
  // recursively handling parentheses. Note: in this part we don't need to
  // track parentheses explicitly (with `Par`) because sibling operators
  // are evaluated left-to-right, the same as our read order.

  def parse1(string: String): Exp =
    def recurse(s: List[Char], exp: Exp, op: Op = Op.None): (Exp, List[Char]) = s match
      case Nil         => (exp, Nil)                 // done
      case ')' :: tail => (exp, tail)                // ascend
      case '*' :: tail => recurse(tail, exp, Op.Mul) // continue
      case '+' :: tail => recurse(tail, exp, Op.Add) // continue
      case chr :: tail =>
        val (currExp, nextTail) =
          if (chr == '(') recurse(tail, NilExp, Op.None) // descend
          else (toNum(chr), tail)
        val nextExp = op match {
          case Op.None => currExp
          case Op.Mul  => Mul(exp, currExp)
          case Op.Add  => Add(exp, currExp)
        }
        recurse(nextTail, nextExp, Op.None) // continue
    recurse(string.toList, NilExp, Op.None)._1

  def part1(input: Input): Long = input.view.map(s => parse1(s).eval).sum

  // Part 2 works by detecting sibling add/mul and forcing the add to take precedent.
  // Happens if `exp` is a Mul when we are performing an Add, and it looks similar
  // to a red-black tree rotation, essentially pushing the Add down the tree so
  // it gets evaluated earlier.
  //   "3 * 2 + 6" would get this treatment
  //   "6 + 3 * 2" would not
  // However for this to work we have to track parentheses explicitly.
  //   "(3 * 2) + 6" must not get re-ordered

  def parse2(string: String): Exp =
    def recurse(s: List[Char], exp: Exp, op: Op = Op.None): (Exp, List[Char]) =
      s match {
        case Nil         => (exp, Nil)                 // done
        case ')' :: tail => (Par(exp), tail)           // ascend
        case '*' :: tail => recurse(tail, exp, Op.Mul) // continue
        case '+' :: tail => recurse(tail, exp, Op.Add) // continue
        case chr :: tail =>
          val (currExp, nextTail) =
            if (chr == '(') recurse(tail, NilExp, Op.None) // descend
            else (toNum(chr), tail)
          val nextExp = op match {
            case Op.None => currExp
            case Op.Mul  => Mul(exp, currExp)
            case Op.Add =>
              exp match {
                case Mul(a, c) => Mul(a, Add(c, currExp)) // rotate!
                case a         => Add(a, currExp)
              }
          }
          recurse(nextTail, nextExp, Op.None) // continue
      }
    recurse(string.toList, NilExp, Op.None)._1

  def part2(input: Input): Long = input.view.map(s => parse2(s).eval).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
