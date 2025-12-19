package aoc2025

import scala.annotation.tailrec
import aoc.Day
import scala.compiletime.ops.double
import scala.math.{min, max}

object Day05 extends Day:

  case class Range(start: Long, end: Long):
    def size: Long                     = end - start + 1
    def contains(n: Long): Boolean     = start <= n && n <= end
    def overlaps(that: Range): Boolean = this.start <= that.end && that.start <= this.end
    // only safe if we know `this` and `that` overlap!
    def union(that: Range) = Range(min(this.start, that.start), max(this.end, that.end))

  object Range:
    def parse(s: String): Option[Range] = s match
      case s"$start-$end" => Some(Range(start.toLong, end.toLong))
      case _              => None

  case class Input(ranges: List[Range], ingredients: List[Long])

  def parse(xs: List[String]): Input =
    val List(chunk0, chunk1) = chunkify(xs)
    Input(
      ranges = chunk0.flatMap(Range.parse),
      ingredients = chunk1.map(_.toLong).toList,
    )

  def part1(input: Input) =
    input.ingredients.count: id =>
      input.ranges.exists(_.contains(id))

  def combineRanges(ranges: List[Range], acc: List[Range] = Nil): List[Range] =
    ranges match
      case Nil => acc
      case head :: tail =>
        tail.partition(_.overlaps(head)) match
          case (Nil, out) => combineRanges(out, head :: acc)
          case (in, out) =>
            val combined = in.foldLeft(head)(_.union(_))
            combineRanges(combined :: out, acc)

  def part2(input: Input) = combineRanges(input.ranges).map(_.size).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
