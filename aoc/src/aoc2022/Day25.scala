package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day25 extends Day:
  type Input = List[String]
  def parse(xs: Iterator[String]): Input = xs.toList

  def digitToInt(x: Char): Int = x match
    case '2' => 2
    case '1' => 1
    case '0' => 0
    case '-' => -1
    case '=' => -2

  def intToDigit(x: Int): Char = x match
    case 2  => '2'
    case 1  => '1'
    case 0  => '0'
    case -1 => '-'
    case -2 => '='

  def fromSnafu(str: String): Long =
    @tailrec def recurse(chars: List[Char], acc: Long): Long = chars match
      case Nil          => acc
      case head :: tail => recurse(tail, acc * 5 + digitToInt(head))
    recurse(str.toList, 0)

  def toSnafu(number: Long): String =
    @tailrec def recurse(curr: Long, acc: String): String =
      if curr == 0 then acc
      else
        val (char, rem) = curr % 5 match
          case 0 => ('0', 0)
          case 1 => ('1', 0)
          case 2 => ('2', 0)
          case 3 => ('=', 1)
          case 4 => ('-', 1)
        recurse((curr / 5) + rem, char +: acc)
    if number == 0 then "0" else recurse(number, "")

  // for fun: calculate a snafu sum directly, without translating it to Int first
  def snafuSum(a: String, b: String): String =
    def recurse(as: List[Char], bs: List[Char], carry: Int = 0, acc: List[Char] = Nil): List[Char] =
      if as.isEmpty && bs.isEmpty then if carry > 0 then '1' :: acc else acc
      else
        val (ai, bi, aTail, bTail) = (as, bs) match
          case (Nil, Nil)               => (0, 0, Nil, Nil)
          case (a :: aTail, Nil)        => (digitToInt(a), 0, aTail, Nil)
          case (Nil, b :: bTail)        => (0, digitToInt(b), Nil, bTail)
          case (a :: aTail, b :: bTail) => (digitToInt(a), digitToInt(b), aTail, bTail)
        val sum = ai + bi + carry
        val (value, nextCarry) =
          if sum < -2 then (sum + 5, -1)
          else if sum > 2 then (sum - 5, 1)
          else (sum, 0)
        recurse(aTail, bTail, nextCarry, intToDigit(value) :: acc)
    recurse(a.toList.reverse, b.toList.reverse).mkString

  def part1(input: Input) =
    toSnafu(input.map(fromSnafu).sum)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
