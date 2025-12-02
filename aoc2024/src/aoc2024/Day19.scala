package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day19 extends Day:

  case class Input(towels: List[List[Char]], patterns: List[List[Char]])

  def parse(xs: List[String]): Input =
    val towels   = xs.head.split(", ").toList.map(_.toList)
    val patterns = xs.drop(2).map(_.toList)
    Input(towels, patterns)

  def isPossible(towels: List[List[Char]], pattern: List[Char]): Boolean =
    def recurse(pattern: List[Char]): Boolean =
      if pattern.isEmpty then true
      else
        towels.iterator
          .filter: x =>
            x == pattern.take(x.size)
          .exists: x =>
            recurse(pattern.drop(x.size))
    recurse(pattern)

  def part1(input: Input) = input.patterns.count(p => isPossible(input.towels, p))

  def allPossible(towels: List[List[Char]], pattern: List[Char]): Long =
    var memo = scala.collection.mutable.Map.empty[List[Char], Long]
    def recurse(pattern: List[Char]): Long =
      memo.getOrElseUpdate(
        pattern,
        if pattern.isEmpty then 1
        else
          towels.iterator
            .filter: x =>
              x == pattern.take(x.size)
            .map: x =>
              recurse(pattern.drop(x.size))
            .sum
      )
    recurse(pattern)

  def part2(input: Input) = input.patterns.map(p => allPossible(input.towels, p)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
