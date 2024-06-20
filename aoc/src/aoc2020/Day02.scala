package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day02 extends Day:
  type Input = Array[Rule]
  def parse(xs: Array[String]): Input = xs.map(Rule.parse)

  type Rule = (Int, Int, Char, String)
  object Rule:
    val regex = raw"(\d+)-(\d+) ([a-z]): (.*)".r
    def parse(s: String): Rule = s match
      case regex(a, b, c, d) => (a.toInt, b.toInt, c.head, d)
      case _                 => throw new Exception(s"Invalid input: $s")

  def part1(input: Input): Int =
    def isValid: Rule => Boolean = { case (min, max, c, p) =>
      val n = p.count(_ == c)
      min <= n && n <= max
    }
    input.count(isValid)

  def part2(input: Input): Int =
    def isValid: Rule => Boolean = { case (i0, i1, c, p) =>
      val m0 = p.size >= i0 && p(i0 - 1) == c
      val m1 = p.size >= i1 && p(i1 - 1) == c
      m0 ^ m1
    }
    input.count(isValid)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
