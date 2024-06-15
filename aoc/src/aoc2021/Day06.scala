package aoc2021

import scala.annotation.tailrec
import scala.collection.View

import aoc.Day

object Day06 extends Day:
  type Fish = Array[Long] // time -> count

  def parse(input: Array[String]): Fish =
    val fish = Array.fill(9)(0L)
    for t <- input(0).split(",") do fish(t.toInt) += 1
    fish

  def step(fish: Fish): Fish =
    val z = fish(0)
    Array.copy(fish, 1, fish, 0, 8)
    fish(6) += z
    fish(8) = z
    fish

  def fishAfterDays(fish: Fish, days: Int): Long =
    View.iterate(fish.clone, days + 1)(step).last.sum

  def part1(initial: Fish) = fishAfterDays(initial, 80)

  def part2(initial: Fish) = fishAfterDays(initial, 256)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
