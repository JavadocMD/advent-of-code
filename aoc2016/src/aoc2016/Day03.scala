package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day03 extends Day:

  val pattern = """\s*(\d+)\s+(\d+)\s+(\d+)\s*""".r

  lazy val input = loadInput().map:
    case pattern(a, b, c) => List(a.toInt, b.toInt, c.toInt)

  lazy val part1 = input.iterator
    .map(_.sorted)
    .count:
      case List(a, b, c) => a + b > c

  lazy val part2 = input.transpose.flatten
    .grouped(3)
    .map(_.sorted)
    .count:
      case List(a, b, c) => a + b > c

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
