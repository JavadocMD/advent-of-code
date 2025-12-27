package aoc2015

import scala.annotation.tailrec
import aoc.Day
import java.security.MessageDigest
import aoc2015.Util._

object Day04 extends Day:

  type Input = String

  def parse(xs: List[String]): Input = xs.head

  def part1(input: Input) =
    val md   = MessageDigest.getInstance("MD5")
    val code = input.getBytes
    Iterator
      .from(1)
      .filter: i =>
        md.update(code)
        md.update(i.toString.getBytes)
        val bs = md.digest()
        // Each hex digit is half a byte, so first 2 1/2 bytes must be zero
        bs(0) == 0 && bs(1) == 0 && (bs(2) & 0xf0) == 0
      .next

  def part2(input: Input) =
    val md   = MessageDigest.getInstance("MD5")
    val code = input.getBytes
    Iterator
      .from(1)
      .filter: i =>
        md.update(code)
        md.update(i.toString.getBytes)
        val bs = md.digest()
        // Each hex digit is half a byte, so first 3 bytes must be zero
        bs(0) == 0 && bs(1) == 0 && bs(2) == 0
      .next

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
