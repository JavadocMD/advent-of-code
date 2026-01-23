package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day18 extends Day:

  lazy val input = loadInput().head.map(_ == '.').toArray // true means safe

  val width = input.size

  def row(prev: Array[Boolean]): Array[Boolean] =
    val next = Array.ofDim[Boolean](width)
    for j <- 0 until width do
      // Simplification: safe if left == right
      val left  = if j > 0 then prev(j - 1) else true
      val right = if j < width - 1 then prev(j + 1) else true
      next(j) = left == right
    next

  def countSafe(numRows: Int): Int =
    Iterator.iterate(input)(row).take(numRows).map(_.count(identity)).sum

  lazy val part1 = countSafe(40)

  lazy val part2 = countSafe(400_000)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
