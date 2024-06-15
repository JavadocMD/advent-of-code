package aoc2021

import scala.annotation.tailrec
import scala.collection.{mutable => m}

// NOTE: solution from https://todd.ginsberg.com/post/advent-of-code/2021/day24/
// This one beat me pretty handily.

import aoc.Day

object Day24 extends Day:

  case class Params(a: Int, b: Int, c: Int)

  type Input = IndexedSeq[Params]
  def parse(xs: Array[String]): Input =
    xs.toIndexedSeq
      .grouped(18)
      .map(ys =>
        val a = ys(4) match
          case s"div z $sa" => sa.toInt
        val b = ys(5) match
          case s"add x $sb" => sb.toInt
        val c = ys(15) match
          case s"add y $sc" => sc.toInt
        Params(a, b, c)
      )
      .toIndexedSeq

  def subroutine(p: Params, z: Long, w: Long): Long =
    if (z % 26 + p.b != w) then ((z / p.a) * 26) + w + p.c
    else z / p.a

  def findMinMax(input: Input): (Long, Long) =
    var zs = Map(0L -> (0L, 0L))
    for p <- input do
      val roundZs = m.Map.empty[Long, (Long, Long)]
      for
        (z, (min, max)) <- zs
        digit           <- 1 to 9
      do
        val newZ = subroutine(p, z, digit)
        if (p.a == 1 || (p.a == 26 && newZ < z))
          val (prevMin, prevMax) = roundZs.get(newZ).getOrElse((Long.MaxValue, Long.MinValue))
          val newMin             = Math.min(prevMin, min * 10 + digit)
          val newMax             = Math.max(prevMax, max * 10 + digit)
          roundZs(newZ) = (newMin, newMax)
      zs = roundZs.toMap
    zs(0)

  final def main(args: Array[String]): Unit =
    val in              = parse(loadInput())
    lazy val (min, max) = findMinMax(in)
    solveP1(() => max)
    solveP2(() => min)
