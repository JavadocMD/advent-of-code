package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day06 extends Day:

  case class Vector2(x: Int, y: Int):
    def +(other: Vector2): Vector2 = Vector2(this.x + other.x, this.y + other.y)
    def rotate: Vector2 =
      this match
        case Direction.E => Direction.S
        case Direction.S => Direction.W
        case Direction.W => Direction.N
        case Direction.N => Direction.E

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = Seq(E, S, W, N)

  sealed trait Tile
  object Tile:
    case object Empty    extends Tile
    case object Obstacle extends Tile

  case class Guard(pos: Vector2, dir: Vector2)

  type Input = (Map[Vector2, Tile], Guard)

  def parse(xs: List[String]): Input =
    var guardPos = Vector2(-1, -1)
    val map = Map.from(
      for
        (line, y) <- xs.zipWithIndex
        (char, x) <- line.zipWithIndex
      yield
        val tile = if (char == '#') then Tile.Obstacle else Tile.Empty
        if (char == '^') then guardPos = Vector2(x, y)
        Vector2(x, y) -> tile
    )
    (map, Guard(guardPos, Direction.N))

  def walk(map: Map[Vector2, Tile], guard: Guard): Set[(Vector2, Vector2)] =
    def recurse(pos: Vector2, dir: Vector2, acc: Set[(Vector2, Vector2)]): Set[(Vector2, Vector2)] =
      val next = pos + dir
      map.get(next) match
        case None             => acc
        case Some(Tile.Empty) => recurse(next, dir, acc + ((next, dir)))
        case Some(Tile.Obstacle) =>
          val nextDir = dir.rotate
          recurse(pos, nextDir, acc + ((pos, nextDir)))
    recurse(guard.pos, guard.dir, Set((guard.pos, guard.dir)))

  def part1(input: Input) =
    val (map, guard) = input
    val visited      = walk(map, guard)
    visited.map(_._1).size

  def walkLoopDetect(map: Map[Vector2, Tile], guard: Guard): Boolean =
    def recurse(pos: Vector2, dir: Vector2, acc: Set[(Vector2, Vector2)]): Boolean =
      val next = pos + dir
      if acc.contains((next, dir)) then true
      else
        map.get(next) match
          case None             => false
          case Some(Tile.Empty) => recurse(next, dir, acc + ((next, dir)))
          case Some(Tile.Obstacle) =>
            val nextDir = dir.rotate
            recurse(pos, nextDir, acc + ((pos, dir)))
    recurse(guard.pos, guard.dir, Set((guard.pos, guard.dir)))

  def part2(input: Input) =
    val (map, guard) = input
    val placements = walk(map, guard)
      .map((pos, dir) => pos + dir)
      .filter(_ != guard.pos)
      .filter(map.get(_) == Some(Tile.Empty))
      .filter(x => walkLoopDetect(map + (x -> Tile.Obstacle), guard))
    placements.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
