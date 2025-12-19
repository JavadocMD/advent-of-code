package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day07 extends Day:

  type Input = List[Set[Int]]

  def parse(xs: List[String]): Input = xs
    .map:
      _.zipWithIndex.filter((c, i) => c == 'S' || c == '^').map(_._2).toSet
    .filter(_.nonEmpty)
    .toList

  def part1(input: Input) =
    var split = 0
    input.tail.foldLeft(input.head):
      case (beams, splitters) =>
        beams.foldLeft(Set.empty[Int]):
          case (acc, b) if splitters.contains(b) =>
            split += 1
            acc.incl(b - 1).incl(b + 1)
          case (acc, b) => acc.incl(b)
    split

  extension [T](m: Map[T, Long])
    def add(key: T, count: Long): Map[T, Long] =
      m.updated(key, count + m.getOrElse(key, 0L))

  def part2(input: Input) =
    input.tail
      .foldLeft(Map.from(input.head.map(_ -> 1L))):
        case (beams, splitters) =>
          beams.foldLeft(Map.empty[Int, Long]):
            case (acc, (b, num)) if splitters.contains(b) => acc.add(b - 1, num).add(b + 1, num)
            case (acc, (b, num))                          => acc.add(b, num)
      .values
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
