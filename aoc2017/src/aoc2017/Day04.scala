package aoc2017

import scala.annotation.tailrec
import aoc.Day

object Day04 extends Day:

  lazy val input = loadInput()

  def isValid(s: String): Boolean =
    s.split(" ")
      .foldLeft(Option(Set.empty[String])):
        case (None, _) => None
        case (Some(acc), curr) =>
          if acc.contains(curr) then None
          else Some(acc + curr)
      .isDefined

  lazy val part1 = input.filter(isValid).size

  def isValid2(s: String): Boolean =
    s.split(" ")
      .foldLeft(Option(Set.empty[String])):
        case (None, _) => None
        case (Some(acc), curr) =>
          val sorted = curr.sorted // canonicalization FTW!
          if acc.contains(sorted) then None
          else Some(acc + sorted)
      .isDefined

  lazy val part2 = input.filter(isValid2).size

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
