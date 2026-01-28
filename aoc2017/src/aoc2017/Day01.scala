package aoc2017

import scala.annotation.tailrec
import aoc.Day

object Day01 extends Day:

  lazy val input = loadInput().head

  lazy val part1 =
    input
      .zip(input.tail + input.head)
      .filter:
        case (a, b) => a == b
      .map(_._1.toString.toInt)
      .sum

  lazy val part2 =
    val (i, j, k) = (0, input.size / 2, input.size)
    input
      .zip(input.slice(j, k) + input.slice(i, j))
      .filter:
        case (a, b) => a == b
      .map(_._1.toString.toInt)
      .sum

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
