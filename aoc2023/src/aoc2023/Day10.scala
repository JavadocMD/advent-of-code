package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day10 extends Day:

  case class Point(x: Int, y: Int):
    def up    = Point(x, y - 1)
    def down  = Point(x, y + 1)
    def left  = Point(x - 1, y)
    def right = Point(x + 1, y)

  sealed trait Pipe
  case object Vertical   extends Pipe // |
  case object Horizontal extends Pipe // -
  case object BendNE     extends Pipe // L
  case object BendNW     extends Pipe // J
  case object BendSW     extends Pipe // 7
  case object BendSE     extends Pipe // F

  def parsePipe(char: Char): Option[Pipe] = char match
    case '|' => Some(Vertical)
    case '-' => Some(Horizontal)
    case 'L' => Some(BendNE)
    case 'J' => Some(BendNW)
    case '7' => Some(BendSW)
    case 'F' => Some(BendSE)
    case _   => None

  case class Input(grid: Map[Point, Pipe], height: Int, width: Int, start: Point):
    def traverse(pipe: Point, from: Point): Point =
      grid(pipe) match
        case Vertical =>
          if from.y < pipe.y then Point(pipe.x, pipe.y + 1)
          else Point(pipe.x, pipe.y - 1)

        case Horizontal =>
          if from.x < pipe.x then Point(pipe.x + 1, pipe.y)
          else Point(pipe.x - 1, pipe.y)

        case BendNE =>
          if from.y < pipe.y then Point(pipe.x + 1, pipe.y)
          else Point(pipe.x, pipe.y - 1)

        case BendNW =>
          if from.y < pipe.y then Point(pipe.x - 1, pipe.y)
          else Point(pipe.x, pipe.y - 1)

        case BendSW =>
          if from.y > pipe.y then Point(pipe.x - 1, pipe.y)
          else Point(pipe.x, pipe.y + 1)

        case BendSE =>
          if from.y > pipe.y then Point(pipe.x + 1, pipe.y)
          else Point(pipe.x, pipe.y + 1)

    def traverseAll(afterStart: Point): Set[Point] =
      // Traverse the whole pipe from `start`, going in the direction indicated by
      // `afterStart` (which must of course be an adjacent point).
      // Returns the set of points in the pipe.
      @tailrec
      def recurse(prev: Point, curr: Point, points: Set[Point]): Set[Point] =
        if curr == this.start then points
        else
          val next = traverse(curr, prev)
          recurse(curr, next, points + next)

      recurse(this.start, afterStart, Set(this.start, afterStart))

  end Input

  def parse(xs: List[String]): Input =
    val grid = Map.from(
      for
        (line, j) <- xs.zipWithIndex
        (char, i) <- line.zipWithIndex
        pipe      <- parsePipe(char)
      yield (Point(i, j) -> pipe),
    )

    val height = xs.size
    val width  = xs(0).size

    val j0 = xs.indexWhere(_.contains("S"))
    val i0 = xs(j0).indexOf("S")
    Input(grid, height, width, start = Point(i0, j0))
  end parse

  def part1(input: Input) =
    // find the points that connect to S...
    // ehh, I could implement this or just find the indices by eyeball
    // println(xs(24).drop(107).take(3).mkString)
    // println(xs(25).drop(107).take(3).mkString)
    // println(xs(26).drop(107).take(3).mkString)
    // start = (108,25)
    // connected = (108,24) (108,26)

    // Walk clockwise and counter-clockwise simultaneously until we meet in the middle.
    // Oops, I now realize this wouldn't have worked if the pipe was an odd length!
    // Could've kept a set of points visited and stopped when hitting a point in the set.
    @tailrec
    def recurse(a: (Point, Point), b: (Point, Point), steps: Int = 1): Int =
      val (preva, nexta) = a
      val (prevb, nextb) = b
      if nexta == nextb then steps
      else
        recurse(
          a = (nexta, input.traverse(nexta, preva)),
          b = (nextb, input.traverse(nextb, prevb)),
          steps + 1,
        )

    recurse(a = (input.start, Point(108, 24)), b = (input.start, Point(108, 26)))
  end part1

  def draw(height: Int, width: Int, grid: Set[Point]): String =
    val letters =
      for j <- 0 until height
      yield for i <- 0 until width
      yield
        if grid.contains(Point(i, j)) then '#'
        else '.'
    letters.map(_.mkString).mkString("\n")

  def flood(isValid: Point => Boolean, start: Point): Set[Point] =
    // Good old flood fill.
    @tailrec
    def recurse(frontier: List[Point], filled: Set[Point]): Set[Point] =
      frontier match
        case Nil => filled
        case curr :: tail =>
          val next = List(curr.up, curr.down, curr.left, curr.right)
            .filter(isValid)
            .filterNot(filled.contains)
          recurse(next ::: tail, filled ++ next)
    recurse(List(start), Set(start))

  def islands(possible: Set[Point], pockets: List[Set[Point]] = Nil): List[Set[Point]] =
    // For all the points in `possible`, determine which are connected to one another.
    // Returns a list of connected point sets.
    if possible.isEmpty then pockets
    else
      val ps = flood(possible.contains, possible.head)
      islands(possible -- ps, ps :: pockets)

  def isInside(loop: Map[Point, Pipe], testPoint: Point): Boolean =
    // do a ray-cast from x=0
    // https://en.wikipedia.org/wiki/Point_in_polygon
    @tailrec
    def recurse(curr: Point, isInside: Boolean): Boolean =
      if curr == testPoint then isInside
      else
        loop.get(curr) match
          case Some(Vertical) => recurse(curr.right, !isInside)
          case Some(BendNW)   => recurse(curr.right, !isInside)
          case Some(BendNE)   => recurse(curr.right, !isInside)
          // Not a loop pipe, or the wrong type of pipe: ignore!
          case _ => recurse(curr.right, isInside)

    recurse(Point(0, testPoint.y), false)
  end isInside

  def part2(input: Input, afterStart: Option[Point] = None) =
    // Traverse the loop to find the pipes we care about.
    // (108,24) is the next point for the real input
    // but can override for tests.
    val next      = afterStart.getOrElse(Point(108, 24))
    val pipes     = input.traverseAll(next)
    val loopPipes = input.grid.filterKeys(pipes.contains).toMap

    // Compute all the obviously external cells
    // by flood-filling from Point(0,0)
    val frame =
      (0 until input.width).map(Point(_, -1)) ++             // top
        (0 until input.width).map(Point(_, input.height)) ++ // bottom
        (0 until input.height).map(Point(-1, _)) ++          // left
        (0 until input.height).map(Point(input.width, _))    // right
    val barriers = pipes ++ frame
    val surround = flood(
      isValid = p => !barriers.contains(p),
      start = Point(0, 0),
    )

    // All remaining cells are fully surrounded by pipe
    // but may be either inside the loop or outside.
    val allPoints = Set.from(
      for
        j <- 0 until input.height
        i <- 0 until input.width
      yield Point(i, j),
    )
    val diff = allPoints -- pipes -- surround

    val pockets = islands(diff)

    pockets.filter(p => isInside(loopPipes, p.head)).map(_.size).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
