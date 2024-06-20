package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day03 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  // Machina... as in 'deus ex'
  class Machina(input: Array[String]):
    val width = input(0).size
    val chars = input.mkString

    def countTrees(rise: Int, run: Int): Int =
      // Each x coordinate.
      val xs = LazyList.from(0, run).map(_ % width)
      // Calculate delta between each x coordinate pair.
      val dxs = pairsIterator(xs).map({ case (x0, x1) => x1 - x0 })
      // Calculate indices basically by adding `run + rise * width`,
      // but we use dx to account for x wrap-around.
      val stride                 = rise * width
      lazy val is: LazyList[Int] = 0 #:: (is zip dxs).map({ case (i, dx) => i + stride + dx })
      // Now just count which chars are trees!
      is.takeWhile(_ < chars.size).count(chars(_) == '#')

  def part1(m: Machina): Int = m.countTrees(1, 3)

  def part2(m: Machina): Long = Seq(
    m.countTrees(1, 1),
    m.countTrees(1, 3),
    m.countTrees(1, 5),
    m.countTrees(1, 7),
    m.countTrees(2, 1)
  ).map(_.toLong).product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(Machina(in)))
    solveP2(() => part2(Machina(in)))
