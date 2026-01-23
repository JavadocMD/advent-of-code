package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._
import aoc2016.Astar

object Day13 extends Day:

  lazy val input = loadInput().head.toLong

  def isWalkable(p: Vector2, favoriteNumber: Long): Boolean =
    val Vector2(x, y) = p
    if x < 0 || y < 0 then false
    else
      val a = favoriteNumber + x * x + 3 * x + 2 * x * y + y + y * y
      java.lang.Long.bitCount(a) % 2 == 0

  val isWalkableMemo = memoize(isWalkable(_, input))

  lazy val part1 =
    Astar
      .gridSearch(
        isWalkable = isWalkableMemo,
        goal = Vector2(31, 39),
        start = Vector2(1, 1),
      )
      .get
      .cost

  def flood(start: Vector2, isWalkable: Vector2 => Boolean): Iterator[Set[Vector2]] =
    val init = (Set(start), Set(start))
    Iterator.unfold(init):
      case (frontier, acc) if frontier.isEmpty => None
      case (frontier, acc) =>
        val nextFrontier = frontier
          .flatMap:
            case p => p.neighbors4
          .filter:
            case p => !acc.contains(p) && isWalkable(p)
        val nextAcc = acc ++ nextFrontier
        Some((nextAcc, (nextFrontier, nextAcc)))

  lazy val part2 =
    flood(Vector2(1, 1), isWalkableMemo)
      .take(50)
      .lastOption
      .get
      .size

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
