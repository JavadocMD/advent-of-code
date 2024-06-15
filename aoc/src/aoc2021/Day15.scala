package aoc2021

import scala.annotation.tailrec
import scala.collection.{mutable => m}

import Grid2D.Point

import aoc.Day

object Day15 extends Day:
  type Input = (Grid, Int) // grid and size (always square)
  type Grid  = Map[Point, Int]

  def parse(xs: Array[String]): Input =
    val entries = for
      (line, y) <- xs.view.zipWithIndex
      (c, x)    <- line.zipWithIndex
    yield Point(x, y) -> c.asDigit
    (Map.from(entries), xs.size)

  // a-star pathfinding algorithm
  def pathfind(grid: Grid, start: Point, goal: Point): Int =
    def h(p: Point): Int = Math.abs(goal.x - p.x) + Math.abs(goal.y - p.y)

    given Ordering[(Point, Int)] with
      def compare(a: (Point, Int), b: (Point, Int)): Int = b._2.compare(a._2)

    val frontier = m.PriorityQueue((start, 0))
    val score    = m.Map(start -> 0)

    while frontier.nonEmpty do
      val (curr, z) = frontier.dequeue()
      if (curr == goal) then frontier.clear()
      for p <- curr.adjacent4.filter(grid.contains) do
        val s = score(curr) + grid(p)
        if s < score.getOrElse(p, Int.MaxValue) then
          score(p) = s
          frontier += ((p, s + h(p)))

    score(goal)

  def part1(input: Input) =
    val (grid, size) = input
    pathfind(grid, Point.zero, Point(size - 1, size - 1))

  def part2(input: Input) =
    val (base, size) = input
    val newSize      = size * 5

    def rescore(s: Int, i: Int, j: Int) = (s - 1 + i + j) % 9 + 1
    val entries =
      for (p, s) <- base.view
      yield for i <- 0 until 5; j <- 0 until 5
      yield Point(p.x + size * i, p.y + size * j) -> rescore(s, i, j)
    val grid = Map.from(entries.flatten)

    pathfind(grid, Point.zero, Point(newSize - 1, newSize - 1))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
