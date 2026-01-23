package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Astar
import aoc2016.Util._

object Day22 extends Day:

  case class Node(x: Int, y: Int, size: Int, used: Int):
    val avail    = size - used
    val location = Vector2(x, y)

  val pattern = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T.*""".r

  lazy val input = loadInput().tail.tail.map:
    case pattern(x, y, s, u) => Node(x.toInt, y.toInt, s.toInt, u.toInt)

  lazy val part1 =
    val viable = for
      a <- input
      b <- input
      if a != b && a.used > 0 && a.used <= b.avail
    yield (a, b)
    viable.size

  lazy val part2 =
    // The example is critical in understanding the construction of this problem;
    // the given nodes can in fact be divided neatly into the three types mentioned --
    // very large full nodes (walls), interchangeable nodes (non-walls), and a single empty node.
    // Importantly there are no walls in the first two rows to account for.

    // So our strategy is:
    // 1. Move the empty space to the left of our desired data with as few steps as possible.
    val topRight  = Vector2(input.maxBy(_.x).x, 0)
    val emptyNode = input.find(_.used == 0).get
    val walkable  = input.filterNot(_.used > emptyNode.size).map(_.location)
    val emptyIntoPosition = Astar
      .gridSearch(walkable.contains, goal = topRight + W.vector, start = emptyNode.location)
      .get
      .cost

    // 2. Repeatedly shift the data left until it gets to (0,0).
    // After moving the data into the empty spot, the empty spot winds up on the right of the data.
    // Cycling the empty back around to the left takes 4 moves.
    val dataMoves  = 1 * topRight.x
    val emptyMoves = 4 * (topRight.x - 1) // don't have to cycle after the last data move!
    emptyIntoPosition + dataMoves + emptyMoves

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
