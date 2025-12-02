package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day02 extends Day:

  type Input = List[List[Int]]

  def parse(xs: List[String]): Input = xs.map(_.split(" ").toList.map(_.toInt))

  def sameSign(a: Int, b: Int): Boolean            = (a > 0 && b > 0) || (a < 0 && b < 0)
  def sameSignOpt(a: Int, b: Option[Int]): Boolean = b.fold(true)(sameSign(a, _))

  def inRange(a: Int): Boolean = a > -4 && a < 4 && a != 0

  def isSafe(levels: List[Int]): Boolean =
    val (_, result) = levels
      .sliding(2)
      .collect { case a :: b :: Nil => b - a }
      .foldLeft((Option.empty[Int], true)) { case ((prev, acc), curr) =>
        (Some(curr), acc && inRange(curr) && sameSignOpt(curr, prev))
      }
    result

  def part1(input: Input) = input.count(isSafe)

  @tailrec
  def isSafeDampened(levels: List[Int], remove: Option[Int] = None): Boolean =
    remove match
      case None                       => isSafe(levels) || isSafeDampened(levels, Some(0))
      case Some(i) if i < levels.size => isSafe(levels.patch(i, Nil, 1)) || isSafeDampened(levels, Some(i + 1))
      case _                          => false

  def part2(input: Input) = input.count(isSafeDampened(_))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
