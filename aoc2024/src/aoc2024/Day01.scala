package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day01 extends Day:

  type Input = (List[Long], List[Long])

  def parse(xs: List[String]): Input =
    val pairs = xs.map { case s"${left}   ${right}" =>
      (left.toLong, right.toLong)
    }
    val lefts  = pairs.map { _._1 }.sorted
    val rights = pairs.map { _._2 }.sorted
    (lefts, rights)

  def part1(input: Input) =
    val (lefts, rights) = input
    lefts.zip(rights).map((a, b) => Math.abs(a - b)).sum

  @tailrec
  def occurrences(xs: List[Long], acc: Map[Long, Int] = Map.empty): Map[Long, Int] = xs match
    case Nil => acc
    case head :: tail =>
      val next = acc.updatedWith(head) {
        case None        => Some(1)
        case Some(value) => Some(value + 1)
      }
      occurrences(tail, next)

  def part2(input: Input) =
    val (lefts, rights) = input
    val occs            = occurrences(rights)
    // Alternatively:
    // val occs = rights.groupMapReduce(identity)(_ => 1)(_ + _)
    lefts.map(x => x * occs.getOrElse(x, 0)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
