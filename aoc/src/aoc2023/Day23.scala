package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day23 extends Day:

  sealed trait Tile
  case object Open           extends Tile // .
  case class Slope(dir: Dir) extends Tile // ><^v

  case class Input(grid: Map[Point, Tile], start: Point, end: Point)

  def parse(xs: List[String]): Input =
    val grid = Map.from(
      for
        (line, j) <- xs.zipWithIndex
        (char, i) <- line.zipWithIndex
        if char != '#'
        p = Point(i, j)
      yield char match
        case '.' => p -> Open
        case '>' => p -> Slope(Right)
        case '<' => p -> Slope(Left)
        case '^' => p -> Slope(Up)
        case 'v' => p -> Slope(Down)
    )
    val h     = xs.size
    val start = Point(xs(0).indexOf('.'), 0)
    val end   = Point(xs(h - 1).indexOf('.'), h - 1)
    Input(grid, start, end)

  def shortcut(curr: Point, input: Input, path: Set[Point] = Set.empty): (Point, Int) =
    // Find the end point after which there are no branches until the true end.
    val walkable = curr.adjacent.filter(p => input.grid.contains(p) && !path.contains(p))
    walkable match
      case next :: Nil => shortcut(next, input, path + next)
      case _           => (curr, path.size - 1)

  def longestHike(input: Input, end: Point): Option[List[Point]] =
    def recurse(curr: Point, prev: Point, path: List[Point]): Option[List[Point]] =
      if curr == end then Some(curr :: path)
      else
        val adjacent = input.grid(curr) match
          case Slope(Left)  => List(curr.left)
          case Slope(Right) => List(curr.right)
          case Slope(Up)    => List(curr.up)
          case Slope(Down)  => List(curr.down)
          case Open         => curr.adjacent
        val next = adjacent.filter(p => input.grid.contains(p) && p != prev)
        next match
          case Nil         => None
          case next :: Nil => recurse(next, curr, curr :: path)
          case next =>
            val hikes = next.flatMap(p => recurse(p, curr, curr :: path))
            if hikes.isEmpty then None
            else Some(hikes.maxBy(_.size))
    recurse(input.start, input.start, Nil)

  def part1(input: Input) =
    val (end, endLength) = shortcut(input.end, input, Set(input.end))
    longestHike(input, end).get.size - 1 + endLength

  type Node = Point
  case class Edge(a: Node, b: Node, length: Int)

  def toGraph(input: Input): (Set[Node], Set[Edge]) =
    import scala.collection.{mutable => m}

    val frontier = m.Queue.from(
      for p <- input.start.adjacent if input.grid.contains(p)
      yield (input.start, input.start, p, 1),
    )

    val visited = m.HashSet(input.start)

    val nodes = m.HashSet[Node](input.start, input.start, input.end)
    val edges = m.HashSet.empty[Edge]

    def explore(start: Node, prev: Point, curr: Point, steps: Int): Unit =
      if nodes.contains(curr) then
        // This is a known node but a new edge
        edges += Edge(start, curr, steps)
      else if !visited.contains(curr) then
        // Walk the edge
        val walkable = curr.adjacent.filter: p =>
          p != prev && input.grid.contains(p)

        walkable match
          case Nil => // dead-end
          case next :: Nil =>
            visited += curr
            explore(start, curr, next, steps + 1)
          case multiple =>
            // This is a node!
            nodes += curr
            edges += Edge(start, curr, steps)
            frontier ++= multiple.map(p => (curr, curr, p, 1))

    while frontier.nonEmpty do
      val (start, prev, curr, steps) = frontier.dequeue()
      explore(start, prev, curr, steps)

    (nodes.toSet, edges.toSet)

  def longestHike2(input: Input): Set[Edge] =
    val (_, graphEdges) = toGraph(input)

    val edgesFrom = graphEdges
      .flatMap(e => e._1 -> e :: e._2 -> e :: Nil)
      .foldLeft(Map.empty[Point, Set[Edge]]):
        case (acc, (node, edge)) =>
          val es = acc.getOrElse(node, Set.empty) + edge
          acc.updated(node, es)

    def recurse(curr: Node, visited: Set[Node], walked: Set[Edge]): Set[Edge] =
      if curr == input.end then walked
      else
        val paths = for
          edge <- edgesFrom(curr).toList
          if !walked.contains(edge)
          next = if curr == edge._1 then edge._2 else edge._1
          if !visited.contains(next)
        yield recurse(next, visited + next, walked + edge)

        if paths.isEmpty then Set.empty
        else paths.maxBy(xs => xs.toList.map(_._3).sum)

    recurse(input.start, Set(input.start), Set.empty)

  def part2(input: Input) =
    longestHike2(input).toList.map(_._3).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
