package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day24 extends Day:

  case class Input(walkable: Map[Vector2, Boolean], start: Vector2, nums: Map[Int, Vector2])

  lazy val input =
    val blueprint = Map.from(
      for
        (row, j)  <- loadInput().zipWithIndex
        (cell, i) <- row.toList.zipWithIndex
      yield Vector2(i, j) -> cell
    )

    val walkable = blueprint
      .flatMap:
        case (_ -> '#') => None
        case (pos -> _) => Some(pos -> true)
      .withDefaultValue(false)

    val nums = blueprint.flatMap:
      case (pos -> cell) if cell.isDigit => Some(cell.toString.toInt -> pos)
      case _                             => None

    Input(walkable, nums(0), nums.removed(0))
  end input

  def distance(pair: (Vector2, Vector2)): Long =
    val (start, goal) = pair
    Astar.gridSearch(input.walkable, goal, start).get.cost

  val distanceMemo = memoize(distance)

  def tripDistance(destinations: List[Vector2]): Long =
    destinations
      .sliding(2)
      .map:
        case start :: goal :: Nil => distanceMemo((start, goal))
      .sum

  lazy val part1 =
    input.nums.values.toList.permutations
      .map(dst => tripDistance(input.start :: dst))
      .min

  lazy val part2 =
    input.nums.values.toList.permutations
      .map(dst => tripDistance(input.start :: dst ::: input.start :: Nil))
      .min

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
