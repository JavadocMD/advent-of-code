package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day25 extends Day:

  type Input = (List[List[Int]], List[List[Int]]) // locks, keys

  def parse(xs: List[String]): Input =
    val locks = xs
      .grouped(8)
      .filter(_.head.head == '#')
      .map(_.dropRight(1).transpose.map(_.count(_ == '#')))
      .toList
    val keys = xs
      .grouped(8)
      .filter(_.head.head == '.')
      .map(_.dropRight(1).transpose.map(_.count(_ == '#')))
      .toList
    (locks, keys)

  def part1(input: Input) =
    val (locks, keys) = input
    val combos = for
      kk <- keys.toIterator
      ll <- locks.toIterator
      if (kk zip ll).forall((k, l) => (k + l) <= 7)
    yield (kk, ll)
    combos.size

  def part2(input: Input) = "MERRY CHRISTMAS"

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
