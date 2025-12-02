package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day03 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs

  def part1(input: Input) =
    val regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    input
      .flatMap(regex.findAllMatchIn)
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum

  def part2(input: Input) =
    val regex = """mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)""".r
    val (_, total) = input
      .flatMap(regex.findAllMatchIn)
      .map(_.matched)
      .foldLeft((true, 0)): // state is: (enabled, acc)
        case ((_, acc), "do()")            => (true, acc)
        case ((_, acc), "don't()")         => (false, acc)
        case ((true, acc), s"mul($a,$b)")  => (true, acc + (a.toInt * b.toInt))
        case ((false, acc), s"mul($a,$b)") => (false, acc)
        case x                             => throw new Exception(s"Unmatched case: ${x}")
    total

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
