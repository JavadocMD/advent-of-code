package aoc2017

import scala.annotation.tailrec
import aoc.Day

object Day02 extends Day:

  lazy val input = loadInput().map:
    case line => line.split("\\s+").map(_.toInt)

  lazy val part1 =
    input
      .map: row =>
        val min = row.min
        val max = row.max
        max - min
      .sum

  lazy val part2 =
    input
      .map: row =>
        val evenDiv = for
          a <- row.iterator
          b <- row
          if a != b && a % b == 0
        yield a / b
        evenDiv.next()
      .sum

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
