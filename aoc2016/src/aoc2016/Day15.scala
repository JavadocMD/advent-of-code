package aoc2016

import scala.annotation.tailrec
import aoc.Day
import scala.math.abs

object Day15 extends Day:

  case class Disc(period: Int, offset: Int)

  lazy val input = loadInput().zipWithIndex.map:
    case (s"Disc $_ has $p positions; at time=0, it is at position $o.", i) =>
      // include the vertical offset here;
      // now finding a time when a dropped capsule would pass through is simply
      // when every disc is at zero according to _this_ offset
      val period = p.toInt
      val offset = (o.toInt + i + 1) % period
      Disc(period, offset)

  def times(discs: List[Disc]): Iterator[Int] =
    // To minimize the times we have to check, look at every time the _largest_ disc is at zero.
    val primeDisc  = discs.maxBy(_.period)
    val otherDiscs = discs.filterNot(_ == primeDisc)
    val t0         = (primeDisc.period - primeDisc.offset) % primeDisc.period
    Iterator
      .from(t0, step = primeDisc.period)
      .filter: t =>
        otherDiscs.forall(d => (t + d.offset) % d.period == 0)

  lazy val part1 = times(input).nextOption.get

  lazy val part2 =
    val discs = input :+ Disc(11, (0 + input.size + 1) % 11)
    times(discs).nextOption.get

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
