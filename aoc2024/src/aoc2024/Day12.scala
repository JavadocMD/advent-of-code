package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day12 extends Day:

  case class Vector2(x: Int, y: Int):
    def +(that: Vector2): Vector2 = Vector2(this.x + that.x, this.y + that.y)
    def adjacent: Seq[Vector2]    = Direction.all.map(_ + this)

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = Seq(E, S, W, N)

    def left(dir: Vector2): Vector2 =
      dir match
        case N => W
        case W => S
        case S => E
        case E => N
        case _ => throw Exception("Invalid direction")

    def leftDiag(dir: Vector2): Vector2 =
      dir match
        case N => Vector2(-1, -1) // NW
        case W => Vector2(-1, 1)  // SW
        case S => Vector2(1, 1)   // SE
        case E => Vector2(1, -1)  // NE
        case _ => throw Exception("Invalid direction")
  end Direction

  case class Region(id: Int, plant: Char, cells: Set[Vector2]):
    def isIn(pos: Vector2): Boolean = cells.contains(pos)
    def area: Int                   = cells.size
    def borders: Iterator[Vector2]  = cells.iterator.flatMap(_.adjacent).filterNot(cells.contains)

    // The number of sides is equal to the number of corners (including interior-edge corners)
    def sides: Int =
      import Direction._

      // Determines if the upper-left vertex of `pos` (when `dir` is "up") should be counted as a corner.
      def notCorner(pos: Vector2, dir: Vector2): Boolean =
        // Two cases:
        // - if we can move forward, not a corner
        // - if we can move left but not forward-left, not a corner
        isIn(pos + dir) || (isIn(pos + left(dir)) && !isIn(pos + leftDiag(dir)))

      // Checks the four vertices of `pos`
      def cornersAt(pos: Vector2): Int = Direction.all.count(!notCorner(pos, _))

      cells.iterator.map(cornersAt).sum
  end Region

  case class Garden(grid: Map[Vector2, Char]):
    def at(pos: Vector2): Char = grid.getOrElse(pos, '.')

    // Flood fill: collect the set of points that are reachable while `predicate` is true.
    def flood(start: Vector2, predicate: Char => Boolean): Set[Vector2] =
      val matches = predicate compose at
      @tailrec
      def recurse(open: Set[Vector2], closed: Set[Vector2]): Set[Vector2] =
        if open.isEmpty then closed
        else
          val head = open.head
          val next = head.adjacent.filter(matches).filterNot(closed.contains)
          recurse(open.tail ++ next, closed + head)
      recurse(Set(start), Set.empty)

    lazy val regions: List[Region] =
      @tailrec
      def recurse(open: Set[Vector2], id: Int = 0, acc: List[Region] = Nil): List[Region] =
        if open.isEmpty then acc
        else
          val start  = open.head
          val plant  = at(start)
          val cells  = flood(start, _ == plant)
          val region = Region(id, plant, cells)
          recurse(open -- cells, id + 1, region :: acc)
      recurse(Set.from(grid.keys))
  end Garden

  type Input = Garden

  def parse(xs: List[String]): Input =
    Garden(
      Map.from(
        for
          (row, y)   <- xs.zipWithIndex
          (plant, x) <- row.zipWithIndex
        yield Vector2(x, y) -> plant
      )
    )

  def cost1(region: Region): Long = region.area * region.borders.size

  def part1(input: Input) = input.regions.map(cost1).sum

  def cost2(garden: Garden)(region: Region): Long = region.area * region.sides

  def part2(input: Input) = input.regions.map(cost2(input)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
