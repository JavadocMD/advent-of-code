package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._

object Day18 extends Day:

  type Input = Set[Vector2]

  def parse(xs: List[String]): Input =
    val lights = for
      (line, y) <- xs.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == '#'
    yield Vector2(x, y)
    lights.toSet

  lazy val input = parse(loadInput().toList)

  def step(state: Set[Vector2]): Set[Vector2] =
    def isOn(v: Vector2): Boolean =
      val ns = v.neighbors8.count(state)
      if state(v) then ns == 2 || ns == 3
      else ns == 3

    val next = for
      y <- (0 until 100).iterator
      x <- (0 until 100).iterator
      v = Vector2(x, y)
      if isOn(v)
    yield v
    next.toSet

  lazy val part1 =
    val result = (0 until 100).foldLeft(input):
      case (state, _) => step(state)
    result.size

  lazy val part2 =
    val corners = Seq(
      Vector2(0, 0),
      Vector2(0, 99),
      Vector2(99, 99),
      Vector2(99, 0),
    )
    val result = (0 until 100).foldLeft(input):
      case (state, _) => step(state) ++ corners
    result.size

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
