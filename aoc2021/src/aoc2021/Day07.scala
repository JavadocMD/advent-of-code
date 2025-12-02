package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day07 extends Day:
  type Input = List[Int]
  def parse(input: Array[String]) = input.head.split(",").toList.map(_.toInt)

  def bestCost(input: List[Int], costs: IndexedSeq[Int]): Int =
    def costForIndex(i: Int) = input.view.map(h => costs(Math.abs(h - i))).sum
    (0 to input.max).view.map(costForIndex).min

  def part1(input: List[Int]) =
    val costs = IndexedSeq.range(0, input.max + 1)
    bestCost(input, costs)

  def part2(input: List[Int]) =
    val costs = IndexedSeq.range(1, input.max + 1).scanLeft(0)(_ + _)
    bestCost(input, costs)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
