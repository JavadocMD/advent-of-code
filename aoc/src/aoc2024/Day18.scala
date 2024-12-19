package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day18 extends Day:

  type Input = List[Vector2]

  def parse(xs: List[String]): Input = xs.map:
    case s"$x,$y" => Vector2.s(x, y)

  case class Vector2(x: Long, y: Long):
    def +(that: Vector2): Vector2      = Vector2(this.x + that.x, this.y + that.y)
    def -(that: Vector2): Vector2      = Vector2(this.x - that.x, this.y - that.y)
    def neighbors: List[Vector2]       = Direction.all.map(this + _)
    def manhattan(that: Vector2): Long = (that.x - this.x).abs + (that.y - this.y).abs
    override def toString(): String    = s"$x,$y"

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toLong, y.toLong)

  given Ordering[Vector2] with
    def compare(a: Vector2, b: Vector2): Int =
      val dx = a.x.compareTo(b.x)
      if dx != 0 then dx else a.y.compareTo(b.y)

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = List(E, S, W, N)

  type Grid = Map[Vector2, Boolean] // is this space walkable?

  case class AstarPath(val score: Long, val path: LazyList[Vector2])

  // TODO: astar doesn't actually seem to benefit us much -- I had a draw method
  // which showed every cell as visited (except the finish).
  // Maybe I can do a marginally faster BFS?
  // And I'd still like to try bidirectional BFS -- this should be a good one for that,
  // looking at the map...

  def astar(grid: Grid, start: Vector2, finish: Vector2): Option[AstarPath] =
    import scala.collection.immutable.TreeSet

    type PriorityQueue[T] = TreeSet[T]
    object PriorityQueue:
      def apply[T: Ordering](value: T): PriorityQueue[T] = TreeSet(value)

    val isWalkable                    = grid.getOrElse(_, false)
    def walkableNeighbors(p: Vector2) = p.neighbors.filter(isWalkable)

    def cost(p: Vector2)      = 1L
    def heuristic(p: Vector2) = p.manhattan(finish)

    case class State(
        val frontier: PriorityQueue[(Vector2, Long)],
        val scoreSeen: Map[Vector2, Long],
        val cameFrom: Map[Vector2, Vector2],
    ):
      def isFailed: Boolean = frontier.isEmpty
      def isSuccess: Boolean = frontier.headOption
        .map((point, _) => point == finish)
        .getOrElse(false)

      def next: State =
        val (curr, _) = frontier.head
        val open =
          for
            point <- walkableNeighbors(curr)
            score = scoreSeen(curr) + cost(point)
            if score < scoreSeen(point)
          yield (point, score)
        State(
          frontier.tail ++ open.map((p, s) => (p, s + heuristic(p))),
          scoreSeen ++ open,
          cameFrom ++ open.map((p, _) => (p, curr)),
        )
    end State

    val initial = State(
      frontier = PriorityQueue((start, 0L)),
      scoreSeen = Map(start -> 0L).withDefaultValue(Long.MaxValue),
      cameFrom = Map.empty[Vector2, Vector2],
    )

    val result = Iterator
      .iterate(initial)(_.next)
      .filter(s => s.isSuccess || s.isFailed)
      .next()

    Option.when(result.isSuccess):
      val score = result.scoreSeen(finish)
      val path = finish #:: LazyList.unfold(finish): prev =>
        result.cameFrom.get(prev).map(x => (x, x))
      AstarPath(score, path)
  end astar

  val start  = Vector2(0, 0)
  val finish = Vector2(70, 70)
  val size   = Vector2(71, 71)

  val emptyGrid = Map.from(
    for
      y <- 0L until size.y
      x <- 0L until size.x
    yield Vector2(x, y) -> true
  )

  def part1(input: Input) =
    val grid = emptyGrid ++ input.take(1024).map(_ -> false)
    astar(grid, start, finish).get.score

  def part2(input: Input) =
    // A linear search is perfectly sufficient for this input,
    // but for fun, let's do part 2 as a binary search.

    def test(index: Int): Boolean =
      // In this case "success" means failing to find a path
      val grid = emptyGrid ++ input.take(index + 1).map(_ -> false)
      astar(grid, start, finish).isEmpty

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
