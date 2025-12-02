package aoc2023

import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoint

import java.math.MathContext
import java.math.RoundingMode
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.math.BigDecimal
import scala.math.BigInt
import aoc.Day

object Day21 extends Day:

  case class Input(walkable: Set[Point], start: Point, grid: Grid)

  def parse(xs: List[String]): Input =
    val walkable = Set.from(
      for
        (line, j) <- xs.zipWithIndex
        (char, i) <- line.zipWithIndex
        if char == '.'
      yield Point(i, j),
    )
    val j0    = xs.indexWhere(_.contains('S'))
    val i0    = xs(j0).indexOf('S')
    val start = Point(i0, j0)
    Input(walkable + start, start, Grid(xs))

  def part1(input: Input) =
    var frontier = Set(input.start)
    for _ <- 0 until 64 do
      val nextFrontier = for
        from <- frontier
        to   <- from.adjacent.filter(input.walkable.contains(_))
      yield to
      frontier = nextFrontier
    frontier.size

  def part2(input: Input) =
    // Smarter people than I realized this could be modeled as a quadratic polynomial
    // fit to a few data points. Below is such a solution.

    def gridMod(point: Point): Point =
      var i = point.x % input.grid.width
      if i < 0 then i += input.grid.width
      var j = point.y % input.grid.height
      if j < 0 then j += input.grid.height
      Point(i, j)

    def isWalkable(point: Point): Boolean =
      input.walkable.contains(gridMod(point))

    // My exploration (deleted) walking over the grid did find patterns in the behavior,
    // such as "cells" (copies of the board) immediately adjacent to the center
    // taking 65 steps to reach, and subsequent cells taking 131 steps to reach.
    // These are related to the size of the board, and helped by the fact that
    // there are no obstacles in the input data for the starting row/column,
    // nor around its edges -- meaning simple manhattan distance fully explains the
    // shortest time to get to a cell.

    // Anyway, that's a lengthy way of explaining the below: let's choose data points
    // at x = 65 and x = (65 + 131 * n) because maybe that will give us a better fit?

    // And how many points do we need? I started with 6 and then once I knew the answer
    // reduced the number of points until I no longer produced that answer -- turns out 3 is fine!
    val strides = (0 until 2).scan(65)((acc, _) => acc + 131)
    var data    = List.empty[(Int, Int)]

    // Collect some data points.
    println("Collecting data points...")
    var frontier = Set(input.start).par // par actually helps quite a bit here! ~8x speedup
    for i <- 1 until strides.max + 1 do
      val nextFrontier = for
        from <- frontier
        to   <- from.adjacent.filter(isWalkable)
      yield to
      if strides.contains(i) then
        val xy = (i, nextFrontier.size)
        println(xy)
        data ::= xy
      frontier = nextFrontier

    // Now fit a curve. Thanks Apache Commons Math...
    println("Fitting curve...")
    val points = data.map({ case (x, y) => WeightedObservedPoint(1.0, x, y) })
    val fitter = PolynomialCurveFitter.create(2)
    val coeff  = fitter.fit(points.asJava).map(BigDecimal(_))

    // Now we just need to evaluate the polynomial and hope our fit is reasonable.
    val x      = BigDecimal(26501365L)
    val result = coeff(2) * x * x + coeff(1) * x + coeff(0)
    result.round(MathContext(15, RoundingMode.HALF_UP)).toLong

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
