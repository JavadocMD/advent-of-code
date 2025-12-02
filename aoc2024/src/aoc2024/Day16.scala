package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day16 extends Day:
  // This solution essentially cribbed from Scala Center and merlinorg
  // https://scalacenter.github.io/scala-advent-of-code/2024/puzzles/day16

  case class Vector2(x: Long, y: Long):
    def +(that: Vector2): Vector2 = Vector2(this.x + that.x, this.y + that.y)
    def -(that: Vector2): Vector2 = Vector2(this.x - that.x, this.y - that.y)
    def rotateCw: Vector2         = Vector2(-y, x)
    def rotateCcw: Vector2        = Vector2(y, -x)

  object Direction:
    val E = Vector2(1, 0)
    val S = Vector2(0, 1)
    val W = Vector2(-1, 0)
    val N = Vector2(0, -1)

  type Grid  = Map[Vector2, Char]
  type Input = (Grid, Vector2, Vector2) // grid, start, end

  def parse(xs: List[String]): Input =
    val grid = Map.from(
      for
        (line, y) <- xs.zipWithIndex
        (tile, x) <- line.zipWithIndex
      yield Vector2(x, y) -> tile
    )
    val start = grid.find((_, x) => x == 'S').get._1
    val end   = grid.find((_, x) => x == 'E').get._1
    (grid, start, end)

  import scala.collection.immutable.TreeMap

  type PriorityQueue[K, V] = TreeMap[K, List[V]]

  object PriorityQueue:
    def apply[K: Ordering, V](kv: (K, V)): PriorityQueue[K, V] =
      val (key, value) = kv
      TreeMap(key -> List(value))

  extension [K, V](queue: PriorityQueue[K, V])
    def enqueue(kv: (K, V)): PriorityQueue[K, V] =
      val (key, value) = kv
      queue.updatedWith(key): xs =>
        Some(value :: xs.getOrElse(Nil))

    def dequeue: (V, PriorityQueue[K, V]) =
      val (priority, values) = queue.head
      if values.size == 1 then (values.head, queue - priority)
      else (values.head, queue + (priority -> values.tail))

    def firstValues: List[V] = queue.valuesIterator.next()
    def firstValue: V        = firstValues.head

  case class Position(cell: Vector2, direction: Vector2)

  case class Move(score: Int, position: Position, path: List[Vector2]):
    def neighbors: List[Move] =
      val Position(cell, dir) = position
      List(
        Move(score + 1, Position(cell + dir, dir), (cell + dir) :: path),
        Move(score + 1000, Position(cell, dir.rotateCw), path),
        Move(score + 1000, Position(cell, dir.rotateCcw), path),
      )

  case class State(grid: Grid, end: Vector2, queue: PriorityQueue[Int, Move], visited: Set[Position]):
    def nextState: State =
      val (curr, rest) = queue.dequeue
      val nextQueue = curr.neighbors
        .filter: next =>
          grid(next.position.cell) != '#' && !visited(next.position)
        .foldLeft(rest): (acc, n) =>
          acc.enqueue(n.score, n)
      State(grid, end, nextQueue, visited + curr.position)

    def solution1: Option[Int] =
      Option(queue.firstValue).filter(_.position.cell == end).map(_.score)

    def solution2: Option[Int] =
      Option.when(queue.firstValue.position.cell == end):
        queue.firstValues.filter(_.position.cell == end).flatMap(_.path).distinct.size

  object State:
    def initial(grid: Grid, start: Vector2, end: Vector2): State =
      State(
        grid,
        end,
        PriorityQueue(0 -> Move(0, Position(start, Direction.E), List(start))),
        visited = Set.empty,
      )

  def part1(input: Input) =
    Iterator
      .iterate(State.initial.tupled(input)):
        _.nextState
      .flatMap(_.solution1)
      .next()

  def part2(input: Input) =
    Iterator
      .iterate(State.initial.tupled(input)):
        _.nextState
      .flatMap(_.solution2)
      .next()

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
