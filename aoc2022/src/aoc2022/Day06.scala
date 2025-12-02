package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day06 extends Day:
  type Input = String
  def parse(xs: String): Input = xs

  def findMarker(s: String, length: Int): Int =
    val i = s
      .sliding(length)
      .indexWhere { s => s.toSet.size == length }
    if i == -1 then -1 else i + length

  def part1(input: Input) = findMarker(input, 4)

  def part2(input: Input) = findMarker(input, 14)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().head)
    part1(in) // warm up cache
    solveP1(() => part1(in))
    solveP2(() => part2(in))
