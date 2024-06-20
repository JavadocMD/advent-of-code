package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day05 extends Day:
  type Input = Array[Int]
  def parse(xs: Array[String]): Input = xs.map(toSeatId).sorted

  // This puzzle is just binary numbers with extra steps...
  def toSeatId(seat: String): Int =
    val binary = seat
      .replaceAll("[FL]", "0")
      .replaceAll("[BR]", "1")
    Integer.parseInt(binary, 2)

  // Input already sorted, so just get the last one.
  def part1(input: Array[Int]): Int = input.last

  // With the seat IDs in order, find the first missing ID.
  def part2(input: Array[Int]): Int = {
    pairsIterator(input)
      .find({ case (a, b) => a + 1 != b })
      .map({ case (a, _) => a + 1 })
      .get
  }

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
