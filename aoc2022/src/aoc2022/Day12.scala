package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day12 extends Day:
  case class Vector2(x: Int, y: Int):
    def minus(other: Vector2) = Vector2(x - other.x, y - other.y)
    def plus(other: Vector2)  = Vector2(x + other.x, y + other.y)
    def manhattanLength       = x.abs + y.abs
    def neighbors: List[Vector2] = List(
      Vector2(x, y + 1), // up
      Vector2(x, y - 1), // down
      Vector2(x - 1, y), // left
      Vector2(x + 1, y)  // right
    )
  object Vector2:
    val zero = Vector2(0, 0)
  end Vector2

  type Path = List[Vector2]
  object Path:
    def stepLength(path: Path): Int = path.size - 1
  end Path

  type Grid[T] = Map[Vector2, T]
  object Grid:
    def distanceToNearest[T](grid: Grid[T], where: T => Boolean)(from: Vector2): Int =
      @tailrec
      def recurse(open: Set[Vector2], closed: Set[Vector2], distance: Int): Int =
        open.find(p => where(grid(p))) match
          case Some(p) => distance
          case None =>
            val nextClosed = (closed ++ open)
            val nextOpen   = open.flatMap(_.neighbors).filter(p => grid.contains(p) && !nextClosed.contains(p))
            recurse(nextOpen, nextClosed, distance + 1)
      recurse(Set(from), Set.empty, 0)
  end Grid

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

  def astar(
      walkableNeighbors: Vector2 => List[Vector2],
      heuristic: Vector2 => Int,
      isDestination: Vector2 => Boolean,
      start: Vector2
  ): Path =
    import scala.collection.{mutable => m}
    given Ordering[(Vector2, Int)] with
      def compare(a: (Vector2, Int), b: (Vector2, Int)): Int = b._2.compare(a._2)

    val frontier = m.PriorityQueue(start -> 0)
    val cameFrom = m.Map.empty[Vector2, Vector2]
    val score    = m.Map(start -> 0).withDefault(p => Int.MaxValue)

    @tailrec
    def pathfind: Vector2 =
      val (curr, _) = frontier.dequeue() // throws if empty, but we can assume the path exists
      if isDestination(curr) then curr
      else
        for
          p <- walkableNeighbors(curr)
          s = score(curr) + 1 if s < score(p)
        do
          cameFrom(p) = curr
          score(p) = s
          frontier += p -> (s + heuristic(p))
        pathfind

    @tailrec
    def reconstructPath(curr: Vector2, path: Path = Nil): Path =
      cameFrom.get(curr) match
        case Some(p) => reconstructPath(p, curr :: path)
        case None    => curr :: path
    reconstructPath(pathfind)
  end astar

  def part1(input: Input) =
    val Input(grid, start, destination) = input
    def walkableNeighbors(point: Vector2): List[Vector2] =
      val pointHeight = grid(point)
      point.neighbors.filter { that =>
        grid.get(that) match
          case Some(thatHeight) => thatHeight <= pointHeight + 1
          case None             => false
      }

    val path = astar(
      walkableNeighbors,
      heuristic = destination.minus(_).manhattanLength,
      isDestination = _ == destination,
      start
    )
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

    val path = astar(
      walkableNeighbors,
      heuristic = memoized(Grid.distanceToNearest(grid, _ == 0)),
      isDestination = grid(_) == 0,
      start = destination
    )
    Path.stepLength(path)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
