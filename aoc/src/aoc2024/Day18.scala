package aoc2024

import scala.annotation.tailrec
import aoc.Day

import aoc2024.Util._

object Day18 extends Day:

  type Input = List[Vector2]

  def parse(xs: List[String]): Input = xs.map:
    case s"$x,$y" => Vector2.s(x, y)

  val start  = Vector2(0, 0)
  val finish = Vector2(70, 70)
  val size   = Vector2(71, 71)

  val emptyGrid = Grid.iterate(size)(_ => true)

  // val pathfind = astar
  def pathfind(grid: Grid[Boolean], start: Vector2, finish: Vector2) =
    bfs(
      grid,
      isWalkable = grid.getOrElse(_, false),
      _ => 1L,
      start,
      finish
    )

  def part1(input: Input) =
    val grid = emptyGrid ++ input.take(1024).map(_ -> false)
    pathfind(grid, start, finish).get.score

  def part2(input: Input) =
    // A linear search is perfectly sufficient for this input,
    // but for fun, let's do part 2 as a binary search.

    def test(index: Int): Boolean =
      // In this case "success" means failing to find a path
      val grid = emptyGrid ++ input.take(index + 1).map(_ -> false)
      pathfind(grid, start, finish).isEmpty

    // Linear search approach:
    // val i = (for
    //   i <- (1024 until input.size).iterator
    //   if test(i)
    // yield i).next()

    // Binary search approach:
    val i = Iterator
      .iterate((1024, input.size)): (low, high) =>
        val mid = low + (high - low) / 2
        if test(mid) then (low, mid) else (mid + 1, high)
      .filter: (low, high) =>
        low == high
      .map(_._1)
      .next()

    input(i).toString()

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
