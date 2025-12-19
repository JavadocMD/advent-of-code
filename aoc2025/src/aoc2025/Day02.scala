package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day02 extends Day:

  type Input = List[(Long, Long)]

  def parse(xs: List[String]): Input =
    xs.head
      .split(",")
      .map:
        case s"$start-$end" => (start.toLong, end.toLong)
      .toList

  def isInvalid(num: Long): Boolean =
    // An invalid ID can be evenly split, and its first and second half are equal.
    val s = num.toString
    val n = s.length
    n % 2 == 0 && s.slice(0, n / 2) == s.slice(n / 2, n)

  def part1(input: Input) =
    input
      .flatMap: (a, b) =>
        (a to b).filter(isInvalid)
      .sum

  def isRepeatedSubsequence(id: String, size: Int): Boolean =
    // Are all the sliding windows of `size` spaced by `size` equal to each other?
    val subs = id.sliding(size = size, step = size)
    val head = subs.next
    subs.forall(_ == head)

  def isInvalid2(num: Long): Boolean =
    // Consider substring lengths from 1 to half the length (inclusive).
    // An invalid ID has at least one repeated subsequence.
    val s = num.toString
    val n = s.length
    (1 to n / 2)
      .filter(n % _ == 0)
      .find(isRepeatedSubsequence(s, _))
      .isDefined

  def part2(input: Input) =
    input
      .flatMap: (a, b) =>
        (a to b).filter(isInvalid2)
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
