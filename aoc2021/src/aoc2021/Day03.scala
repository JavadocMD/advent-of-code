package aoc2021

import scala.annotation.tailrec
import Binary._

import aoc.Day

object Day03 extends Day:
  type Input = List[String]
  def parse(xs: Array[String]): Input = xs.toList

  def part1(input: Input) =
    given bits: BitSize(input.head.size)
    val n = input.size

    // 1 is the most common digit in position `i` is 1 (or tied for most common), 0 otherwise
    def popularDigit(i: Int): Int = (input.count(_.charAt(i) == '1') >= n / 2).toBinary

    val gamma   = bits.range.foldLeft(0) { case (g, i) => (g << 1) | popularDigit(i) }
    val epsilon = Binary.invert(gamma)
    gamma * epsilon

  def part2(input: Input) =
    @tailrec
    def bitFilter(xs: List[String], test: (Int, Int) => Boolean, i: Int = 0): String =
      if (xs.size == 1) xs.head
      else
        val (zeros, ones) = xs.partition(_.charAt(i) == '0')
        val keep          = if test(zeros.size, ones.size) then zeros else ones
        bitFilter(keep, test, i + 1)

    val o2  = Binary.parse(bitFilter(input, _ > _))  // keep zeros if most common
    val co2 = Binary.parse(bitFilter(input, _ <= _)) // keep zeros if least common, or tied
    o2 * co2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
