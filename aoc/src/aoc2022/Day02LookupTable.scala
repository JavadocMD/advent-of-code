package aoc2022

import scala.annotation.tailrec
import aoc.Day

object Day02LookupTable extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Input) =
    def score(s: String): Int = s match
      case "A X" => 4
      case "A Y" => 8
      case "A Z" => 3
      case "B X" => 1
      case "B Y" => 5
      case "B Z" => 9
      case "C X" => 7
      case "C Y" => 2
      case "C Z" => 6
    input.foldLeft(0)(_ + score(_))

  def part2(input: Input) =
    def score(s: String): Int = s match
      case "A X" => 3 // lose -> scissors
      case "A Y" => 4 // draw -> rock
      case "A Z" => 8 // win -> paper
      case "B X" => 1 // lose -> rock
      case "B Y" => 5 // draw -> paper
      case "B Z" => 9 // win -> scissors
      case "C X" => 2 // lose -> paper
      case "C Y" => 6 // draw -> scissors
      case "C Z" => 7 // win -> rock
    input.foldLeft(0)(_ + score(_))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
