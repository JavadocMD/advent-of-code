package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day04 extends Day:
  type Input = List[(Range, Range)]
  def parse(xs: Array[String]): Input =
    xs.toList.map { case s"${a}-${b},${c}-${d}" =>
      (Range.inclusive(a.toInt, b.toInt), Range.inclusive(c.toInt, d.toInt))
    }

  def part1(input: Input) =
    input.count { (a, b) =>
      (a.start <= b.start && a.end >= b.end) || (b.start <= a.start && b.end >= a.end)
    }

  def part2(input: Input) =
    input.count { (a, b) =>
      (a.start <= b.start && a.end >= b.start) || (a.start <= b.end && a.end >= b.end) ||
      (b.start <= a.start && b.end >= a.start) || (b.start <= a.end && b.end >= a.end)
    }

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
