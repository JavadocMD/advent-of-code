package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day04 extends Day:

  case class Vector2(x: Int, y: Int):
    def +(other: Vector2): Vector2 = Vector2(this.x + other.x, this.y + other.y)

  type Input = Map[Vector2, Char]

  object Direction:
    val E  = Vector2(1, 0)
    val SE = Vector2(1, 1)
    val S  = Vector2(0, 1)
    val SW = Vector2(-1, 1)
    val W  = Vector2(-1, 0)
    val NW = Vector2(-1, -1)
    val N  = Vector2(0, -1)
    val NE = Vector2(1, -1)

    val all = Seq(E, SE, S, SW, W, NW, N, NE)

  def parse(xs: List[String]): Input =
    Map
      .from(
        for
          (line, y)   <- xs.zipWithIndex
          (letter, x) <- line.zipWithIndex
        yield Vector2(x, y) -> letter
      )
      .withDefaultValue('.')

  def isMatch(input: Input, start: Vector2, dir: Vector2): Boolean =
    input(start + dir) == 'M' &&
      input(start + dir + dir) == 'A' &&
      input(start + dir + dir + dir) == 'S'

  def part1(input: Input) =
    val matches = for
      (pos, letter) <- input
      if letter == 'X'
      dir <- Direction.all
      if isMatch(input, pos, dir)
    yield 1
    matches.sum

  def isMatch2(input: Input, start: Vector2): Boolean =
    val nwse = (input(start + Direction.NW), input(start + Direction.SE))
    val nesw = (input(start + Direction.NE), input(start + Direction.SW))
    (
      (nwse == ('M', 'S') || nwse == ('S', 'M')) &&
      (nesw == ('M', 'S') || nesw == ('S', 'M'))
    )

  def part2(input: Input) =
    val matches = for
      (pos, letter) <- input
      if letter == 'A'
      if isMatch2(input, pos)
    yield 1
    matches.sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
