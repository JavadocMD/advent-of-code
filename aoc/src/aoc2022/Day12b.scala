package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day12b extends Day:
  import Grid2D._

  case class Input(grid: Grid[Int], start: Vector2, destination: Vector2)

  def parse(xs: Iterator[String]): Input =
    var grid        = Map.empty[Vector2, Int]
    var start       = Vector2.zero
    var destination = Vector2.zero
    for
      (line, j) <- xs.zipWithIndex
      (c, i)    <- line.zipWithIndex
      p = Vector2(i, j)
    do
      c match
        case 'S' =>
          start = p
          grid += p -> 0
        case 'E' =>
          destination = p
          grid += p -> 25
        case _ =>
          grid += p -> (c.toInt - 'a'.toInt)
    Input(grid, start, destination)
  end parse

  def part1(input: Input) =
    val Input(grid, start, destination) = input
    def walkableNeighbors(point: Vector2): List[Vector2] =
      val pointHeight = grid(point)
      point.neighbors.filter { that =>
        grid.get(that) match
          case Some(thatHeight) => thatHeight <= pointHeight + 1
          case None             => false
      }

    val path = Astar.create(
      walkableNeighbors,
      heuristic = destination.minus(_).manhattanLength,
      isDestination = _ == destination
    )(start)
    Path.stepLength(path)

  def part2(input: Input) =
    val Input(grid, start, destination) = input
    def walkableNeighbors(point: Vector2): List[Vector2] =
      val pointHeight = grid(point)
      point.neighbors.filter { that =>
        grid.get(that) match
          case Some(thatHeight) => thatHeight >= pointHeight - 1
          case None             => false
      }

    val path = Astar.create(
      walkableNeighbors,
      heuristic = memoized(p => Grid.search(grid, _ == 0)(p).minus(p).manhattanLength),
      isDestination = grid(_) == 0
    )(destination)
    Path.stepLength(path)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
