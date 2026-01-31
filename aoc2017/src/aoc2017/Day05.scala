package aoc2017

import scala.annotation.tailrec
import aoc.Day

object Day05 extends Day:

  lazy val input = loadInput().view.map(_.toInt).toIndexedSeq

  @tailrec
  def run(curr: Int, jumps: IndexedSeq[Int], steps: Int = 0): Int =
    if curr < 0 || curr >= jumps.size then steps
    else
      val j = jumps(curr)
      run(curr + j, jumps.updated(curr, j + 1), steps + 1)

  lazy val part1 = run(0, input)

  def run2(jumps: IndexedSeq[Int]): Int =
    // Mutability isn't really necessary, but it shaves this from 1000ms to 40ms.
    val mjumps = jumps.toArray()

    @tailrec
    def recurse(curr: Int, steps: Int = 0): Int =
      if curr < 0 || curr >= mjumps.length then steps
      else
        val j     = mjumps(curr)
        val delta = if j >= 3 then -1 else 1
        mjumps(curr) = j + delta
        recurse(curr + j, steps + 1)

    recurse(0)

  lazy val part2 = run2(input)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
