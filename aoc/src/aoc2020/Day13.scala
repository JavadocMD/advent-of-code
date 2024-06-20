package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day13 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Input): Int =
    val arrival = input(0).toInt
    val busses  = input(1).split(",").filter(_ != "x").map(_.toInt)

    def wait(id: Int) = (arrival / id + 1) * id - arrival

    val (id, w) = busses.map(x => (x, wait(x))).minBy(_._2)
    id * w

  // Each pair is an ID and its offset. After finding where two numbers "align",
  // those can be combined by multiplying the numbers and using their common offset.
  def search(aPair: (Long, Long), bPair: (Long, Long)): (Long, Long) =
    val (a, aOffset) = aPair
    val (b, bOffset) = bPair
    var na           = aOffset
    while ((na + bOffset) % b != 0) {
      na += a
    }
    (a * b, na)

  def part2(input: Input): Long =
    val busses = input(1)
      .split(",")
      .toSeq
      .zipWithIndex
      .filter({ case (s, i) => s != "x" })
      .map({ case (s, i) => (s.toLong, i.toLong) })
    busses.reduceLeft(search)._2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
