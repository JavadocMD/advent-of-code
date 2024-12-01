package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day20 extends Day:
  import TwoDee.{Vector => Vector2}
  import ThreeDee.{Vector => Vector3}

  /* Note: inspection of the input shows that each portal pair has one inner and one outer side. */
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def isLetter(c: Char): Boolean = 65 <= c && c <= 90

  sealed trait Side
  object Side:
    case object Outer extends Side
    case object Inner extends Side

  class Maze(val grid: Array[String]):
    val xBounds = grid(0).length
    val yBounds = grid.length

    def get(x: Int, y: Int): Option[Char] =
      if (x < 0 || x >= xBounds) None
      else if (y < 0 || y >= yBounds) None
      else Some(grid(y)(x))

    def getLetter(x: Int, y: Int): Option[(Char, Vector2)] =
      for { c <- get(x, y) if isLetter(c) } yield (c, Vector2(x, y))

    def getWalkable(x: Int, y: Int): Option[Vector2] =
      for { c <- get(x, y) if c == '.' } yield Vector2(x, y)

    def isWalkable(v: Vector2): Boolean = get(v.x, v.y) == Some('.')
    def isWalkable(v: Vector3): Boolean = get(v.x, v.y) == Some('.')

    /** Given the position of the first letter of a portal label, true if that portal is on the outer edge of the maze,
      * false if on the inner.
      */
    def side(v: Vector2): Side =
      if (
        v.x == 0 || v.x == xBounds - 2 ||
        v.y == 0 || v.y == yBounds - 2
      ) Side.Outer
      else Side.Inner

  def gridIterator(xs: Array[String]): Stream[(Int, Int, Char)] = for {
    (row, y)  <- Stream.from(xs).zip(Stream.from(0))
    (char, x) <- Stream.from(row).zip(Stream.from(0))
  } yield (x, y, char)

  case class Portal(label: String, point: Vector2, side: Side)

  def findPortals(maze: Maze): List[Portal] =

    /** Parse a single label, given the position of its first letter. */
    def parseAt(point: Vector2): (Char, Vector2, Vector2) = {
      val Vector2(x, y) = point
      // Will return the second letter, its position, and the associated walkable position.
      // Look left-and-right...
      val h = for {
        (c2, p1) <- maze.getLetter(x + 1, y)
        p2       <- maze.getWalkable(x + 2, y) orElse maze.getWalkable(x - 1, y)
      } yield (c2, p1, p2)
      // Look up-and-down...
      val v = for {
        (c2, p1) <- maze.getLetter(x, y + 1)
        p2       <- maze.getWalkable(x, y + 2) orElse maze.getWalkable(x, y - 1)
      } yield (c2, p1, p2)
      // One of these should come back `Some`.
      (h orElse v) match {
        case Some(res) => res
        case None      => throw new Exception(s"parseNode failed: ($x,$y)")
      }
    }

    var portals = List.empty[Portal]

    var skips = Set.empty[Vector2] // so we can skip the second letter of labels
    for {
      (x, y, c1) <- gridIterator(maze.grid)
      v = Vector2(x, y)
      if isLetter(c1) && !skips.contains(v)
    } {
      // We're guaranteed to hit the first letter of a label.
      // The rest of the node is either horizontal or vertically adjacent.
      val (c2, skip, point) = parseAt(v)
      portals ::= Portal(s"$c1$c2", point, maze.side(v))
      skips += skip
    }

    portals

  def part1(maze: Maze, _portals: List[Portal]): Int =
    import scala.collection.{mutable => m}
    val start = _portals.find(_.label == "AA").get.point
    val goal  = _portals.find(_.label == "ZZ").get.point

    // Map from each portal to its sister portal (if any).
    val portals: Map[Vector2, Vector2] = Map.from(
      _portals
        .groupBy(_.label)
        .values
        .flatMap({
          case a :: b :: Nil => (a.point -> b.point) :: (b.point -> a.point) :: Nil
          case _             => Nil
        })
    )

    // Neighbor set includes portal traversal.
    def neighbors(v: Vector2): Seq[Vector2] = {
      v.neighbors.filter(maze.isWalkable) ++ portals.get(v).toSeq
    }

    val distance = m.Map(start -> 0)
    val frontier = m.Queue(start)
    while (frontier.nonEmpty) {
      val prev = frontier.dequeue()
      val dist = distance(prev) + 1
      if (dist >= distance.getOrElse(goal, Int.MaxValue)) {
        frontier.clear() // halt early! we can't get any better than we've got
      } else {
        neighbors(prev).filterNot(distance.contains).foreach { curr =>
          distance(curr) = dist
          frontier += curr
        }
      }
    }

    distance(goal)

  def part2(maze: Maze, _portals: List[Portal]): Int =
    import scala.collection.{mutable => m}
    val start = Vector3.withZ(_portals.find(_.label == "AA").get.point, 0)
    val goal  = Vector3.withZ(_portals.find(_.label == "ZZ").get.point, 0)

    // Map each pair of portals by entry point.
    val portals: Map[Vector2, (Portal, Portal)] = Map.from(
      _portals
        .groupBy(_.label)
        .values
        .flatMap({
          case a :: b :: Nil => (a.point -> (a, b)) :: (b.point -> (b, a)) :: Nil
          case _             => Nil
        })
    )

    def getPortal(v: Vector3): Option[Vector3] = {
      portals
        .get(Vector3.to2D(v))
        .flatMap({ case (a, b) =>
          val z = v.z + (if (a.side == Side.Inner) 1 else -1)
          if (z < 0) None else Some(Vector3.withZ(b.point, z))
        })
    }

    // Neighbor set includes portal traversal.
    def neighbors(v: Vector3): Seq[Vector3] = {
      v.neighborsXY.filter(maze.isWalkable) ++ getPortal(v).toSeq
    }

    val distance = m.Map(start -> 0)
    val frontier = m.Queue(start)
    while (frontier.nonEmpty) {
      val prev = frontier.dequeue()
      val dist = distance(prev) + 1
      if (dist >= distance.getOrElse(goal, Int.MaxValue)) {
        frontier.clear() // halt early! we can't get any better than we've got
      } else {
        neighbors(prev).filterNot(distance.contains).foreach { curr =>
          distance(curr) = dist
          frontier += curr
        }
      }
    }

    distance(goal)

  def part1(input: Input): Long =
    val maze    = new Maze(input)
    val portals = findPortals(maze)
    part1(maze, portals) // 432

  def part2(input: Input): Long =
    val maze    = new Maze(input)
    val portals = findPortals(maze)
    part2(maze, portals) // 5214

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
