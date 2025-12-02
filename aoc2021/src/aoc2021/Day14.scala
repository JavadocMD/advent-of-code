package aoc2021

import scala.annotation.tailrec
import scala.collection.View
import scala.collection.{mutable => m}

import aoc.Day

object Day14 extends Day:
  // This solution thanks to inspiration from
  // https://todd.ginsberg.com/post/advent-of-code/2021/day14/
  // Much simpler -- just track the number of pairs directly.

  type Rules       = Map[(Char, Char), Char]
  type Pairs       = Map[(Char, Char), Long]
  type Occurrences = Map[Char, Long]
  case class Input(template: String, rules: Rules)

  def parse(xs: Array[String]): Input =
    val (xs0, xs1) = xs.view.span(_.nonEmpty)
    Input(
      xs0.head,
      xs1.drop(1).map({ case s"$p -> $i" => (p(0), p(1)) -> i(0) }).toMap
    )

  def toPairs(xs: String): Pairs = xs.zip(xs.tail).groupMapReduce(identity)(_ => 1L)(_ + _)

  def step(rules: Rules)(pairs: Pairs): Pairs =
    val acc = m.Map.empty[(Char, Char), Long].withDefault(_ => 0L)
    for (pair @ (x, y), count) <- pairs do
      val z = rules(pair)
      acc((x, z)) += count
      acc((z, y)) += count
    acc.toMap

  def occurrences(starting: String, pairs: Pairs): Occurrences =
    val acc = m.Map.empty[Char, Long].withDefault(_ => 0L)
    for ((x, _), count) <- pairs do acc(x) += count
    acc(starting.last) += 1
    acc.toMap

  def solution(input: Input, times: Int): Long =
    val initial = toPairs(input.template)
    val result  = repeatedly(step(input.rules))(initial, times)
    val occs    = occurrences(input.template, result)
    val max     = occs.values.max
    val min     = occs.values.min
    max - min

  def part1(input: Input) = solution(input, 10)

  def part2(input: Input) = solution(input, 40)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
