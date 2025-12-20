package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.min

object Day02 extends Day:

  type Input = List[(Long, Long, Long)]

  def sortedTriple[T](a: T, b: T, c: T)(using Ordering[T]): (T, T, T) =
    val t0 :: t1 :: t2 :: Nil = List(a, b, c).sorted: @unchecked
    (t0, t1, t2)

  def parse(xs: List[String]): Input = xs.map:
    case s"${l}x${w}x${h}" => sortedTriple(l.toLong, w.toLong, h.toLong)

  def part1(input: Input) =
    input
      .map:
        case (l, w, h) => 2 * l * w + 2 * w * h + 2 * h * l + l * w
      .sum

  def part2(input: Input) =
    input
      .map:
        case (l, w, h) => 2 * (l + w) + l * w * h
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
