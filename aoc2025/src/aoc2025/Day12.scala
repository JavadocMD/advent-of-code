package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day12 extends Day:

  case class Region(width: Int, height: Int, number: List[Int]):
    val volume = width * height
    def presentVolume(using presents: List[Present]) =
      (number zip presents).map((N, P) => N * P.volume).sum

  case class Present(pattern: Vector[Vector[Char]]):
    val volume = pattern.map(_.count(_ == '#')).sum

  case class Input(regions: List[Region], presents: List[Present])

  def parse(xs: List[String]): Input =
    val chunks = chunkify(xs)
    val regionsChunk = chunks.last
    val presentChunks = chunks.dropRight(1)
    Input(
      regions = regionsChunk.collect:
        case s"${w}x${h}: $nums" =>
          Region(
            width = w.toInt,
            height = h.toInt,
            number = nums.split(" ").map(_.toInt).toList,
          ),
      presents = presentChunks
        .map: lines =>
          Present(lines.tail.map(_.toVector).toVector),
    )

  def part1(input: Input) =
    given List[Present] = input.presents
    input.regions
      .filter(r => r.presentVolume <= r.volume)
      .size

  def part2(input: Input) = "MERRY CHRISTMAS"

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
