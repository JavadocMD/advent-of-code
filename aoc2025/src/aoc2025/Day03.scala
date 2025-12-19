package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day03 extends Day:

  type Input = List[List[Int]]

  def parse(xs: List[String]): Input =
    xs.map: line =>
      line.map(_.asDigit).toList

  @tailrec
  def maxJoltage(bats: List[Int], num: Int, acc: List[Int] = Nil): Long =
    // done if we've found the required number of digits
    if acc.size == num then acc.reverse.mkString.toLong
    else
      val r = num - acc.size - 1    // digits remaining to be found after this step
      val d = bats.dropRight(r).max // find best digit, reserving at least `r` digits for future
      val i = bats.indexOf(d)       // drop our selected digit and everything left of it
      maxJoltage(bats.drop(i + 1), num, d :: acc)

  def part1(input: Input) = input.map(maxJoltage(_, 2)).sum

  def part2(input: Input) = input.map(maxJoltage(_, 12)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
