package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day01 extends Day:
  type Input = List[Int]
  def parse(xs: Array[String]): Input = xs.toList.map(_.toInt)

  def part1(input: List[Int]) =
    // Non-recursive solution:
    // input.sliding(2).count(xs => xs.head < xs.tail.head)

    // Or using the Util function, which is slightly slower (fn call overhead?)
    // input.pairwiseFoldLeft(0)((r, a, b) => if a < b then r + 1 else r)

    @tailrec
    def recurse(xs: List[Int], last: Int, count: Int): Int = xs match
      case Nil => count
      case head :: tail =>
        val nextCount = if head > last then count + 1 else count
        recurse(tail, head, nextCount)
    recurse(input.tail, input.head, 0)

  def part2(input: List[Int]) =
    // Non-recursive solution:
    // val windows = input.sliding(3).map(_.sum).toList
    // part1(windows)

    // Or using the Util function, which is slightly slower (fn call overhead?)
    // input.pairwiseFoldLeft(0)((r, a, b) => if a < b then r + 1 else r, 3)

    @tailrec
    def recurse(prevs: List[Int], xs: List[Int], count: Int): Int = xs match
      case Nil => count
      case head :: tail =>
        val last      = prevs.head
        val nextCount = if head > last then count + 1 else count
        recurse(prevs.tail ::: head :: Nil, tail, nextCount)
    recurse(input.take(3), input.drop(3), 0)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))

// with sliding: part 1 (36ms), part 2 (15ms)
// recursive in part 1 only: part 1 (1ms), part 2 (47ms)
// both recursive: part 1 (1ms), part 2 (1ms)
