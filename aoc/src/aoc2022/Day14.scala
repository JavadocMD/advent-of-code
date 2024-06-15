package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day14 extends Day:
  case class Vector2(x: Int, y: Int):
    def down      = Vector2(x, y + 1)
    def downLeft  = Vector2(x - 1, y + 1)
    def downRight = Vector2(x + 1, y + 1)

  type Grid[T] = Map[Vector2, T]

  enum Tile:
    case Rock
    case Sand
  import Tile._

  type Input = Grid[Tile]

  def range(a: Int, b: Int): Range = if a <= b then a to b else b to a

  def parse(xs: Iterator[String]): Input =
    def parseLine(line: String): Iterator[Vector2] = line
      .split(" -> ")
      .map { case s"${IntP(x)},${IntP(y)}" => Vector2(x, y) }
      .iterator

    val cells = for
      line      <- xs
      Seq(a, b) <- parseLine(line).sliding(2)
      y         <- range(a.y, b.y)
      x         <- range(a.x, b.x)
    yield Vector2(x, y) -> Rock
    cells.toMap

  def findLowest(map: Grid[Tile]): Int = map.maxBy((v, _) => v.y)._1.y

  def dropSand1(lowestRock: Int, at: Vector2)(map: Grid[Tile]): Option[Grid[Tile]] =
    @tailrec def recurse(pos: Vector2): Option[Grid[Tile]] =
      if pos.y > lowestRock then None
      else if !map.contains(pos.down) then recurse(pos.down)
      else if !map.contains(pos.downLeft) then recurse(pos.downLeft)
      else if !map.contains(pos.downRight) then recurse(pos.downRight)
      else Some(map + (pos -> Sand))
    recurse(at)

  def countSand(initial: Grid[Tile], dropSand: Grid[Tile] => Option[Grid[Tile]]): Int =
    Iterator
      .unfold(initial) { dropSand(_).map(x => (x, x)) }
      .size

  val sandEntry = Vector2(500, 0)

  def part1(input: Input) =
    val f = dropSand1(findLowest(input), sandEntry)
    countSand(input, f)

  def dropSand2(floor: Int, at: Vector2)(map: Grid[Tile]): Option[Grid[Tile]] =
    val lowestSand = floor - 1 // as low as sand can get (hitting the floor)
    @tailrec def recurse(pos: Vector2): Option[Grid[Tile]] =
      if pos.y == lowestSand then Some(map + (pos -> Sand))
      else if !map.contains(pos.down) then recurse(pos.down)
      else if !map.contains(pos.downLeft) then recurse(pos.downLeft)
      else if !map.contains(pos.downRight) then recurse(pos.downRight)
      else Some(map + (pos -> Sand))
    if map.contains(at) then None
    else recurse(at)

  def part2(input: Input) =
    val f = dropSand2(findLowest(input) + 2, sandEntry)
    countSand(input, f)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
