package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day01 extends Day:

  case class Move(turn: Turn, dist: Int)

  lazy val input = loadInput().head
    .split(", ")
    .view
    .map:
      case s"L$dist" => Move(Left, dist.toInt)
      case s"R$dist" => Move(Right, dist.toInt)
    .toList

  lazy val part1 =
    val src: (Dir4, Vector2) = (N, Vector2.zero)
    val dst = input.foldLeft(src):
      case ((dir, pos), move) =>
        val nextDir = dir.turn90(move.turn)
        val nextPos = pos + nextDir.vector * move.dist
        (nextDir, nextPos)
    dst._2.manhattan(Vector2.zero)

  @tailrec
  def firstDouble(
      moves: List[Move],
      dir: Dir4 = N,
      pos: Vector2 = Vector2.zero,
      visited: Set[Vector2] = Set.empty,
  ): Option[Vector2] = moves match
    case Nil => None
    case head :: tail =>
      val nextDir  = dir.turn90(head.turn)
      val nextPos  = pos + nextDir.vector * head.dist
      val visiting = (1 to head.dist).map(d => pos + nextDir.vector * d)
      visiting.find(visited.contains) match
        case found @ Some(_) => found
        case None            => firstDouble(tail, nextDir, nextPos, visited ++ visiting)

  lazy val part2 = firstDouble(input).get.manhattan(Vector2.zero)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
