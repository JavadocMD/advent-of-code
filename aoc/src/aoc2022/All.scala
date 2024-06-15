package aoc2022

import scala.util.Try

object All:

  val all_days = Seq(
    Day01,
    Day02,
    Day03,
    Day04,
    Day05,
    Day06,
    Day07,
    Day08,
    Day09,
    Day10,
    Day11,
    Day12,
    Day13,
    Day14,
    Day15,
    Day16,
    Day17,
    Day18,
    // Day19, // This one takes too long to run...
    Day20,
    Day21,
    Day22,
    Day23,
    Day24,
    Day25,
  )

  final def main(args: Array[String]): Unit =
    all_days foreach { case day =>
      println(s"> ${day.getClass.getSimpleName}:")
      day.main(args)
      println()
    }
