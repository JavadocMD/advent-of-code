package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day05 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs

  def part1(input: Input) =
    val regx1 = "(ab)|(cd)|(pq)|(xy)".r.unanchored
    val regx2 = "(.)\\1".r.unanchored
    val regx3 = "[aeiou].*[aeiou].*[aeiou]".r.unanchored
    input.iterator
      .filterNot(regx1.matches)
      .filter(regx2.matches)
      .filter(regx3.matches)
      .size

  def part2(input: Input) =
    val regx1 = "(.).\\1".r.unanchored
    val regx2 = "(..).*\\1".r.unanchored
    input.iterator
      .filter(regx1.matches)
      .filter(regx2.matches)
      .size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
