package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day19 extends Day:
  import IntcodeComputer as C

  type Input = Scan
  def parse(xs: Array[String]): C.Program = C.parseMemory(xs.head)

  /** A function which scans an x,y coordinate and returns true if it's affected by the tractor beam.
    */
  type Scan = (Long, Long) => Boolean

  def beamScanner(program: C.Program): Scan =
    val state = C.State(program)
    def beam(x: Long, y: Long): Boolean = {
      val sf = C.runIOSync1(state, x :: y :: Nil)
      sf.output.head == 1
    }
    beam

  // Draw the beam for funsies.
  def draw(scan: Scan): String =
    val grid = Array.tabulate(50, 50) { case (y, x) =>
      if (scan(x, y)) '#' else '.'
    }
    grid.map(row => row.mkString).mkString("\n")

  // Returns the number of points affected by the tractor beam in the bounded area.
  // Speed things up by tracking the left and right edge of the beam separately.
  // Takes advantage of the fact the beam is linear -- so the x coords are
  // guaranteed to be monotonically non-decreasing.
  def countBeam(xBounds: Int, yBounds: Int, scan: Scan): Int =
    var count, y, xL, xR = 0
    while (y < yBounds) {
      // advance x-left until beam
      while (xL < xBounds && !scan(xL, y)) xL += 1
      // advance x-right until no beam
      if (xR < xL) xR = xL + 1
      while (xR < xBounds && scan(xR, y)) xR += 1
      // number affected in this row is the difference of the x coords
      count += xR - xL
      y += 1
    }
    count

  // Find the closest (to (0,0)) position which will fit a
  // `size` x `size` square entirely within the beam.
  def findSquare(size: Int, scan: Scan): (Int, Int) =
    var done = false
    var x, y = 0
    while (!done) {
      // scan for bottom-left corner (look ahead by size)
      while (!scan(x, y + size - 1)) x += 1
      // check if top-right corner is in beam
      done = scan(x + size - 1, y)
      y += 1 // will overshoot by one
    }
    (x, y - 1) // return top-left corner

  def part1(scan: Input): Long = countBeam(50, 50, scan)

  def part2(scan: Input): Long =
    val (x, y) = findSquare(100, scan) // (1509,773)
    10_000L * x + y

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(beamScanner(in)))
    solveP2(() => part2(beamScanner(in)))
