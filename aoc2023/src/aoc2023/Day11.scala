package aoc2023

import scala.annotation.tailrec
import scala.collection.SortedSet
import aoc2023.{Grid, Point}
import aoc.Day

object Day11 extends Day:

  case class Input(grid: Grid, galaxies: List[Point])

  def parse(xs: List[String]): Input =
    val grid = Grid(xs)
    val galaxies = grid.pointsIterator
      .filter({ case (p, c) => c == '#' })
      .map({ case (p, c) => p })
      .toList
    Input(grid, galaxies)

  def emptyLines(input: Input): (Set[Int], Set[Int]) =
    val xs   = input.galaxies.map(_.x).toSet
    val ys   = input.galaxies.map(_.y).toSet
    val rows = (0 until input.grid.height).filterNot(ys.contains).toSet
    val cols = (0 until input.grid.width).filterNot(xs.contains).toSet
    (rows, cols)

  def countInRange(a: Int, b: Int, items: Set[Int]): Long =
    val (min, max) = (math.min(a, b), math.max(a, b))
    (min + 1 until max).count(items.contains).toLong

  @tailrec
  def pairwiseDist(
      galaxies: List[Point],
      emptyRows: Set[Int],
      emptyCols: Set[Int],
      factor: Long,
      distances: List[Long],
  ): List[Long] =
    galaxies match
      case Nil         => distances
      case head :: Nil => distances
      case head :: tail =>
        val ds = tail.map(other =>
          head.manhattanDist(other) +
            (factor - 1) * countInRange(head.x, other.x, emptyCols) +
            (factor - 1) * countInRange(head.y, other.y, emptyRows),
        )
        pairwiseDist(tail, emptyRows, emptyCols, factor, ds ::: distances)

  def part1(input: Input) =
    val (emptyRows, emptyCols) = emptyLines(input)
    pairwiseDist(input.galaxies, emptyRows, emptyCols, 2L, Nil).sum

  def part2(input: Input) =
    val (emptyRows, emptyCols) = emptyLines(input)
    pairwiseDist(input.galaxies, emptyRows, emptyCols, 1_000_000L, Nil).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
