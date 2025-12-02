package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day03 extends Day:
  type Input = List[String]
  def parse(xs: Array[String]): Input = xs.toList

  // 'A' is 65, so n-65+27 yields 27-52
  // 'a' is 97, so n-97+1 yields 1-26
  def score(c: Char): Int =
    val n = c.toInt
    if n < 97 then n - 38 else n - 96

  def findCommon2(a: String, b: String): Char =
    a.find { x =>
      b.find(y => x == y).isDefined
    }.get

  def part1(input: Input) =
    input
      .map { s =>
        val n = s.length / 2
        val a = s.substring(0, n)
        val b = s.substring(n)
        findCommon2(a, b)
      }
      .map(score)
      .sum

  def findCommon3(a: String, b: String, c: String): Char =
    a.find { x =>
      b.find { y =>
        x == y && c.find(z => y == z).isDefined
      }.isDefined
    }.get

  def part2(input: Input) =
    input
      .grouped(3)
      .map {
        case List(a, b, c) => findCommon3(a, b, c)
        case _             => throw new Exception("invalid case")
      }
      .map(score)
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
