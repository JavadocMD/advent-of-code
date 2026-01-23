package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day16 extends Day:

  lazy val input = loadInput().head.map(_ == '1').toArray

  def dragon(input: Array[Boolean]): Array[Boolean] =
    val n      = input.size
    val m      = n * 2 + 1
    val output = Array.ofDim[Boolean](m)
    Array.copy(input, 0, output, 0, n)
    output(n) = false
    var i = 0
    var j = m - 1
    while i < n do
      output(j) = !output(i)
      i += 1
      j -= 1
    output

  def checksum(input: Array[Boolean]): Array[Boolean] =
    val n     = input.size / 2
    var check = Array.ofDim[Boolean](n)

    var i = 0
    var j = 0
    while i < input.size do
      val bit = !(input(i) ^ input(i + 1))
      check(j) = bit
      i += 2
      j += 1

    if n % 2 == 1 then check
    else checksum(check)

  def stringify(input: Array[Boolean]): String =
    input
      .map:
        case true  => '1'
        case false => '0'
      .mkString

  lazy val part1 =
    val targetSize = 272
    Iterator
      .iterate(input)(dragon)
      .find(_.size >= targetSize)
      .map(_.take(targetSize))
      .map(checksum)
      .map(stringify)
      .get

  lazy val part2 =
    val targetSize = 35651584
    Iterator
      .iterate(input)(dragon)
      .find(_.size >= targetSize)
      .map(_.take(targetSize))
      .map(checksum)
      .map(stringify)
      .get

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
