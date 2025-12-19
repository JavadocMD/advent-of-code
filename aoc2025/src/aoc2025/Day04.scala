package aoc2025

import scala.annotation.tailrec
import aoc.Day
import aoc2025.Util.Vector2

object Day04 extends Day:

  type Input = Set[Vector2]

  def parse(xs: List[String]): Input =
    Set.from(
      for
        (line, y) <- xs.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char == '@'
      yield Vector2(x, y)
    )

  def part1(input: Input) =
    input
      .filter: pos =>
        pos.neighbors8.count(input.contains) < 4
      .size

  @tailrec
  def remove(grid: Set[Vector2], number: Int = 0): Int =
    val (removed, remaining) = grid.partition: pos =>
      pos.neighbors8.count(grid.contains) < 4
    removed.size match
      case 0 => number
      case n => remove(remaining, number + n)

  def part2(input: Input) = remove(input)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
