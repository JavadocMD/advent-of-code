package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day05 extends Day:

  case class Range(start: Long, length: Long):
    val end = start + length

  case class AlmanacMap(dst: Long, src: Long, length: Long):
    val src_end = src + length

    def matches(value: Long): Boolean = value >= src && value < (src + length)

    def convert(value: Long): Long = dst + (value - src)

  case class AlmanacSection(maps: List[AlmanacMap]):
    def convert(value: Long): Long =
      maps.find(_.matches(value)) match
        case Some(m) => m.dst + (value - m.src)
        case None    => value

    @tailrec
    final def convertRange(range: Range, maps: List[AlmanacMap] = maps, curr: List[Range] = Nil): List[Range] =
      maps match
        case Nil =>
          // No more maps to consider, remaining Range translates directly
          range :: curr
        case m :: rest if range.end <= m.src =>
          // Range completes before this map
          range :: curr
        case m :: rest if range.start >= m.src_end =>
          // Range starts after this map, keep searching
          convertRange(range, rest, curr)
        case m :: rest if range.end <= m.src_end =>
          // Range is consumed by this map
          val converted = Range(m.convert(range.start), range.length)
          converted :: curr
        case m :: rest =>
          // Range is split by this map
          val overlapLength = m.src_end - range.start
          val converted     = Range(m.convert(range.start), overlapLength)
          val remainder     = Range(m.src_end, range.length - overlapLength)
          convertRange(remainder, rest, converted :: curr)

  case class Input(seeds: List[Long], sections: List[AlmanacSection]):
    def convert(seed: Long): Long =
      sections.foldLeft(seed)((value, section) => section.convert(value))

  def blocks(xs: Array[String]): List[List[String]] =
    @tailrec
    def recurse(lines: List[String], curr: List[String], result: List[List[String]]): List[List[String]] =
      lines match
        case Nil                          => (curr.reverse :: result).reverse
        case head :: tail if head.isBlank => recurse(tail, Nil, curr.reverse :: result)
        case head :: tail                 => recurse(tail, head :: curr, result)
    recurse(xs.toList, Nil, Nil)

  def parse(xs: Array[String]): Input = blocks(xs) match
    case first :: rest =>
      val seeds = first.head match
        case s"seeds: $rest" => rest.split(" ").toList.map(_.toLong)

      val sections = rest.map({ block =>
        AlmanacSection(
          block
            .drop(1)
            .map({ case s"$dst $src $length" =>
              AlmanacMap(dst.toLong, src.toLong, length.toLong)
            })
            .sortBy(_.src),
        )
      })

      Input(seeds, sections)

    case _ => throw Exception("Invalid input.")

  def part1(input: Input) =
    input.seeds.map(input.convert).min

  def part2(input: Input) =
    val seedRanges = input.seeds.grouped(2).map(xs => Range(xs.head, xs.last)).toList

    val locRanges = input.sections.foldLeft(seedRanges)({ case (ranges, section) =>
      ranges.flatMap(section.convertRange(_))
    })

    locRanges.map(_.start).min

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
