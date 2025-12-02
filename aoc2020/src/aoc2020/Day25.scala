package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day25 extends Day:
  type Input = Array[Long]
  def parse(xs: Array[String]): Input = xs.map(_.toLong)

  def mulMod(multiplicand: Long, multiplier: Long, modulus: Long): Long = {
    var res = 0L
    var a   = multiplicand % modulus
    var b   = multiplier
    while (b > 0) {
      if ((b & 1) > 0) {
        res = (res + a) % modulus
      }
      a = (2 * a) % modulus
      b >>= 1
    }
    res
  }

  val m = 20201227L

  def findLoopSize(a: Long): Int =
    // Find loop size of the first input using the default subject number (7).
    var loops = 0
    var value = 1L
    while (value != a) {
      value = mulMod(value, 7L, m)
      loops += 1
    }
    loops

  def transform(b: Long, loops: Int): Long =
    // Using the second input as the subject number, transform `loops` times.
    var value = 1L
    var i     = 0
    while (i < loops) {
      value = mulMod(value, b, m)
      i += 1
    }
    value

  def part1(input: Input): Long =
    val l = findLoopSize(input(0))
    transform(input(1), l)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
