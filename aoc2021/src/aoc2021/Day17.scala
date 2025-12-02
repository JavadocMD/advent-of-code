package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day17 extends Day:
  type Input = VectorBounds
  def parse(xs: Array[String]): Input = xs.head match
    case s"target area: x=$x0..$x1, y=$y0..$y1" =>
      VectorBounds(x0.toInt, x1.toInt, y0.toInt, y1.toInt)

  def isInBounds(initialVelocity: Vector, bounds: Input): (Boolean, Int) =
    import VectorBounds.Ops._
    def sim(pos: Vector, vel: Vector, maxY: Int): (Boolean, Int) =
      val nextPos  = pos + vel
      val nextVel  = Vector(Math.max(0, vel.x - 1), vel.y - 1)
      val nextMaxY = Math.max(maxY, nextPos.y)
      if nextPos.isWithin(bounds) then (true, nextMaxY)
      else if nextPos.isRightOf(bounds) || nextPos.isBelow(bounds) then (false, nextMaxY)
      else sim(nextPos, nextVel, nextMaxY)
    sim(Vector.zero, initialVelocity, 0)

  type Solution = (Vector, Int)

  def solutions(bounds: Input): Seq[Solution] =
    for
      vx <- 1 until 160
      vy <- -150 to 150
      initialVelocity = Vector(vx, vy)
      (success, maxY) = isInBounds(initialVelocity, bounds)
      if success
    yield (initialVelocity, maxY)

  def part1(solutions: Seq[Solution]) = solutions.map(_._2).max

  def part2(solutions: Seq[Solution]) = solutions.size

  final def main(args: Array[String]): Unit =
    val in      = parse(loadInput())
    lazy val xs = solutions(in)
    solveP1(() => part1(xs))
    solveP2(() => part2(xs))
