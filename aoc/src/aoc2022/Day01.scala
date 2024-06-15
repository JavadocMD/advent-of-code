package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day01 extends Day:
  type Input = Vector[Int]
  def parse(xs: Array[String]): Input = splitGroups(xs, Integer.parseInt).map(_.sum)

  def part1(input: Input) = input.max

  def part2(input: Input) = input.sorted(Ordering[Int].reverse).take(3).sum

  // Special fun: solve in a single read-through
  def onePass(xs: Array[String]): (Int, Int) =
    // Essentially we want to keep the best three sums as we go.
    val emptyTopThree = Vector(0, 0, 0)

    def topThree(ns: Vector[Int], n: Int): Vector[Int] =
      // Keep the best three out of four numbers.
      // Somewhat crude, perhaps, but seems faster than `append` then `sorted` then `take(3)`.
      ns.indexWhere(_ < n) match
        case 0 => Vector(n, ns(0), ns(1))
        case 1 => Vector(ns(0), n, ns(1))
        case 2 => Vector(ns(0), ns(1), n)
        case _ => ns

    // Recursive pattern match on all rows, with groups separated by blank lines.
    // Within a group, compute its sum.
    // After each group, compute the top three sums.
    @tailrec
    def recurse(rows: List[String], soln: Vector[Int], group: Int): Vector[Int] =
      rows match
        case Nil          => topThree(soln, group)
        case "" :: tail   => recurse(tail, topThree(soln, group), 0)
        case head :: tail => recurse(tail, soln, group + Integer.parseInt(head))

    val Vector(a, b, c) = recurse(xs.toList, emptyTopThree, 0)
    (a, a + b + c)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))

    onePass(loadInput())
