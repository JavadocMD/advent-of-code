package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day17 extends Day:

  case class Input(grid: Grid, scores: Map[Point, Int])

  def parse(xs: List[String]): Input =
    val g = Grid(xs)
    val s = g.pointsIterator
      .map: (p, c) =>
        p -> c.asDigit
      .toMap
    Input(g, s)

  // speed represents how far we've moved in a straight line
  case class Position(point: Point, dir: Dir, speed: Int):
    def validMoves(canTurn: Boolean, canStraight: Boolean): List[Position] =
      val ms =
        if !canTurn then Nil
        else turns(dir).map(d => Position(point.go(d), d, 1))
      if !canStraight then ms
      else Position(point.go(dir), dir, speed + 1) :: ms

  type Path = List[Position]

  class Astar2[P](
      walkableNeighbors: P => List[P],
      score: P => Int,
      heuristic: P => Int,
      isDestination: P => Boolean,
  ):
    type Path = List[P]

    def search(starts: List[P]): Path =
      import scala.collection.{mutable => m}
      given Ordering[(P, Int)] with
        def compare(a: (P, Int), b: (P, Int)): Int = b._2.compare(a._2)

      val startPairs = starts.map((_, 0))
      val frontier   = m.PriorityQueue.from(startPairs)
      val cameFrom   = m.Map.empty[P, P]
      val scoreSeen  = m.Map.from(startPairs).withDefault(p => Int.MaxValue)

      @tailrec
      def pathfind: P =
        val (curr, _) = frontier.dequeue() // throws if empty, but we can assume the path exists
        if isDestination(curr) then curr
        else
          for
            p <- walkableNeighbors(curr)
            s = scoreSeen(curr) + score(p) if s < scoreSeen(p)
          do
            cameFrom(p) = curr
            scoreSeen(p) = s
            frontier += p -> (s + heuristic(p))
          pathfind

      @tailrec
      def reconstructPath(curr: P, path: Path = Nil): Path =
        cameFrom.get(curr) match
          case Some(p) => reconstructPath(p, curr :: path)
          case None    => curr :: path

      reconstructPath(pathfind)

  end Astar2

  def heatLoss(input: Input, path: Path): Int =
    // Drop the starting point.
    path.tail.map(x => input.scores(x.point)).sum

  def stringify(input: Input, path: Path): String =
    import scala.collection.{mutable as m}
    val (w, h) = input.grid.size
    val chars  = m.ArrayBuffer.fill(h, w)('.')
    for pos <- path do
      val Point(i, j) = pos.point
      val c = pos.dir match
        case Up    => '^'
        case Down  => 'v'
        case Left  => '<'
        case Right => '>'
      chars(j)(i) = c
    chars.map(_.mkString).mkString("\n")

  def part1(input: Input) =
    val (w, h) = input.grid.size
    val finish = Point(w - 1, h - 1)

    val path = Astar2[Position](
      walkableNeighbors = pos =>
        pos
          .validMoves(canTurn = true, canStraight = pos.speed < 3)
          .filter(x => input.grid.contains(x.point)),
      score = pos => input.scores(pos.point),
      heuristic = pos => pos.point.manhattanDist(finish),
      isDestination = pos => pos.point == finish,
    ).search(
      starts = List(
        Position(Point.zero, Right, 0),
        Position(Point.zero, Down, 0),
      ),
    )

    heatLoss(input, path)

  def part2(input: Input) =
    val (w, h) = input.grid.size
    val finish = Point(w - 1, h - 1)

    val path = Astar2[Position](
      walkableNeighbors = pos =>
        pos
          .validMoves(canTurn = pos.speed >= 4, canStraight = pos.speed < 10)
          .filter(x => input.grid.contains(x.point)),
      score = pos => input.scores(pos.point),
      heuristic = pos => pos.point.manhattanDist(finish),
      isDestination = pos => pos.point == finish && pos.speed >= 4,
    ).search(
      starts = List(
        Position(Point.zero, Right, 0),
        Position(Point.zero, Down, 0),
      ),
    )

    heatLoss(input, path)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
