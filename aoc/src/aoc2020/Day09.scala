package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.{immutable => i, mutable => m}

import aoc.Day

object Day09 extends Day:
  type Input = Array[Long]
  def parse(xs: Array[String]): Input = xs.map(_.toLong)

  // Is there a combination of 2 numbers in the buffer which sum to this number?
  def isValid(x: Long, buffer: Seq[Long]) = buffer.combinations(2).exists(_.sum == x)

  def findFirstInvalid(nums: Seq[Long], preambleSize: Int): Option[Long] =
    val (preamble, rest) = nums.splitAt(preambleSize)
    val buffer           = m.Queue.from(preamble) // the last `preambleSize` numbers
    rest.find { x =>
      // Test x.
      val valid = isValid(x, buffer.toSeq)
      // Update the buffer.
      buffer.dequeue()
      buffer.enqueue(x)
      // Return result.
      !valid
    }

  def part1(input: Input): Long = findFirstInvalid(input, 25).get

  def findContiguousSet(input: Seq[Long], invalid: Long): Option[(Int, Int)] =
    val iInvalid = input.indexOf(invalid)
    var (i0, i1) = (0, 1) // the indices of our contiguous sum range
    var done     = false
    while (!done) {
      var sum = input(i0) // the current sum
      while (!done && i1 < iInvalid) {
        sum += input(i1)
        if (sum == invalid) done = true
        else if (sum > invalid) i1 = iInvalid // early quit inner loop; advance i0
        else i1 += 1                          // advance i1
      }
      if (!done) {
        i0 += 1
        i1 = i0 + 1
      }
    }
    if (done) Some((i0, i1)) else None

  def part2(input: Input, invalid: Long): Long =
    val (i0, i1) = findContiguousSet(input, invalid).get
    val xs       = input.slice(i0, i1 + 1)
    xs.min + xs.max

  final def main(args: Array[String]): Unit =
    val in   = parse(loadInput())
    val ans1 = solveP1(() => part1(in))
    solveP2(() => part2(in, ans1))
