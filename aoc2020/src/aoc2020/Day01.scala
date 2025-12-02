package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day01 extends Day:
  type Input = Array[Int]
  def parse(xs: Array[String]): Input = xs.map(_.toInt)

  def part1(numbers: Input): Int =
    val results = for {
      a <- numbers
      b <- numbers
      if (a != b && a + b == 2020)
    } yield a * b
    results.head

  def part2(numbers: Input): Int =
    val results = for {
      a <- numbers
      b <- numbers
      c <- numbers
      if (a != b && b != c && a + b + c == 2020)
    } yield a * b * c
    results.head

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
