package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day06 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Array[String]): Int =
    batchIterator[Set[Char]](_.toSet, _ union _)(input).map(_.size).sum

  def part2(input: Array[String]): Int =
    batchIterator[Set[Char]](_.toSet, _ intersect _)(input).map(_.size).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput(preserveBlanks = true))
    solveP1(() => part1(in))
    solveP2(() => part2(in))
