package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day06 extends Day:

  lazy val input = loadInput()

  lazy val freqs = input
    .map(_.toList)
    .transpose
    .map:
      _.groupMapReduce(identity)(_ => 1)(_ + _)

  lazy val part1 = freqs
    .map:
      _.maxBy(_._2)._1
    .mkString

  lazy val part2 = freqs
    .map:
      _.minBy(_._2)._1
    .mkString

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
