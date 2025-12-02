package aoc2023

import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc.Day

object Day01 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  val p1c1 = "^.*?([0-9]).*$".r
  val p1c2 = "^.*([0-9]).*?$".r

  def get_char(pattern: Regex, line: String): String =
    line match
      case pattern(c) => c
      case _          => throw Exception("No digit!")

  def part1(input: Input) =
    val nums = for line <- input.view yield
      val digit1 = get_char(p1c1, line)
      val digit2 = get_char(p1c2, line)
      s"$digit1$digit2".toLong
    nums.sum

  val p2c1 = "^.*?([0-9]|one|two|three|four|five|six|seven|eight|nine).*$".r
  val p2c2 = "^.*([0-9]|one|two|three|four|five|six|seven|eight|nine).*?$".r

  def convert(s: String): Long =
    s match
      case "one"   => 1
      case "two"   => 2
      case "three" => 3
      case "four"  => 4
      case "five"  => 5
      case "six"   => 6
      case "seven" => 7
      case "eight" => 8
      case "nine"  => 9
      case _       => s.toLong

  def part2(input: Input) =
    val nums = for line <- input.view yield
      val digit1 = convert(get_char(p2c1, line))
      val digit2 = convert(get_char(p2c2, line))
      digit1 * 10 + digit2
    nums.sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
